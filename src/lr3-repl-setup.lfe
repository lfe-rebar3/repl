(defmodule lr3-repl-setup
  (export all))

(include-lib "clj/include/compose.lfe")

(defun set-name (state)
  (rebar_api:debug "Setting up name ..." '())
  (let* ((`#(,opts ,_) (rebar_state:command_parsed_args state))
         (name (proplists:get_value 'name opts))
         (sname (proplists:get_value 'sname opts)))
    (rebar_api:debug "\tShort name: ~p; Long name: ~p" `(,sname ,name))
    (case `#(,name ,sname)
      (#(undefined undefined)
        'ok)
      (`#(,name undefined)
        (check-epmd (net_kernel:start `(,name longname))))
      (`#(undefined ,sname)
        (check-epmd (net_kernel:start `(,sname shortname))))
      (`#(,_ ,_)
        (rebar_api:abort
          "Cannot have both short and long node names defined."
          '())))))

(defun set-paths (state)
  (rebar_api:debug "Setting up paths ..." '())
  (rebar_api:debug "\tAdding app paths ..." '())
  (-> state
      (rebar_state:code_paths 'all_deps)
      (add-paths))
  (rebar_api:debug "\tAdding test paths ..." '())
  (-> state
      (lr3-repl-util:get-test-paths)
      (add-paths)))

(defun prep-repl ()
    (rebar_api:debug "Prep'ing REPL ..." '())
    ;; Scan all processes for any with references to the old user and save them
    ;; to update later
    (let ((needs-update (lists:filtermap #'needs-update?/1 (erlang:processes))))
      ;; Start a new shell (this also starts a new user under the correct group)
      (rebar_api:debug "\tStarting LFE REPL process ..." '())
      (spawn #'lfe_shell:server/0)
      ;; Wait until processes have been registered
      (wait-until-user-started 3000)
      ;; Set any process that had a reference to the old user's group leader to
      ;; the new user process. Catch the race condition when the Pid exited
      ;; after the liveness check.
      (catch
        (lists:foreach #'update-group-leader/1 needs-update))
      (try
        (progn
          ;; Enable error_logger's tty output
          (error_logger:swap_handler 'tty)
          ;; Disable the simple error_logger (which may have been added multiple
          ;; times). removes at most the error_logger added by init and the
          ;; error_logger added by the tty handler
          (remove-error-handler 3))
        (catch
          (`#(,err ,msg ,trace) ; may fail with custom loggers
            (rebar_api:debug "\tLogger changes failed for ~p:~p (~p)"
                             `(,err ,msg ,trace))
            'hope-for-best)))))

(defun needs-update ()
  (lists:filtermap #'needs-update?/1 (erlang:processes)))

(defun needs-update? (pid)
  (case (update-check pid)
    ('true `#(true ,pid))
    ('false 'false)))

(defun update-check (pid)
  (->> pid
       (erlang:process_info)
       (proplists:get_value 'group_leader)
       (== (whereis 'user))))

(defun update-group-leader
  ((pid)
    (if (is_process_alive pid)
      (progn
        (rebar_api:debug "Updating group leader ..." '())
        (erlang:group_leader (whereis 'user) pid)))))


(defun simulate-proc-lib ()
  (rebar_api:debug "Simulating proc lib ..." '())
  (let ((fake-parent (spawn_link (lambda () (timer:sleep 'infinity)))))
    (put '$ancestors `(,fake-parent))
    (put '$initial_call #(rebar_agent init 1))))

(defun check-epmd
  ((`#(error #(#(shutdown #(,_ net_kernel #(EXIT nodistribution))))))
    (rebar_api:error (++ "Erlang Distribution failed, falling back to "
                         "nonode@nohost. Verify that epmd is running and try "
                         "again.")
                     '()))
  ((_)
   'ok))

(defun remove-error-handler
  ((0)
    (rebar_api:warn "Unable to remove simple error_logger handler" '()))
  ((n)
    (case (gen_event:delete_handler 'error_logger 'error_logger '())
      (#(error module_not_found)
        'ok)
      (`#(error_logger ,_)
        (remove-error-handler (- n 1))))))

(defun wait-until-user-started
  ((0)
    (rebar_api:abort "Timeout exceeded waiting for `user` to register itself"
                     '()))
  ((timeout)
    (case (whereis 'user)
      ;; if user is not yet registered wait a tenth of a second and try again
      ('undefined
        (timer:sleep 100)
        (wait-until-user-started (- timeout 100)))
      (_ 'ok))))

(defun add-paths (paths)
  (rebar_api:debug "\tPaths: ~p" `(,paths))
  (lists:map #'add-path/1 paths))

(defun add-path (path)
  (rebar_api:debug "\tAdding path ~p ..." `(,path))
  (code:add_patha path))

(defun register-agent (pid)
  (rebar_api:debug "Registering rebar_agent ..." '())
  (case (register 'rebar_agent pid)
    ('true
      'ok)
    (_
      (let ((msg "Failed to register rebar agent process"))
        (rebar_api:error msg '())
        (error #(registration-error msg))))))
