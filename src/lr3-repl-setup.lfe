(defmodule lr3-repl-setup
  (export all))

(include-lib "clj/include/compose.lfe")

(defun set-name (state)
  (rebar_api:debug "Setting up name ..." '())
  (let* ((`#(,opts ,_) (rebar_state:command_parsed_args state))
         (name (proplists:get_value 'name opts))
         (sname (proplists:get_value 'sname opts)))
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
  (let ((paths (rebar_state:code_paths state 'all_deps)))
    (rebar_api:debug "Paths: ~p" `(,paths))
    (lists:foreach
      (lambda (x)
        (rebar_api:debug "Adding path: ~p" `(,x)))
      paths)
    ;; Add lib dirs to path
    (code:add_pathsa paths))
    ;; Add project app test paths
  (add-test-paths state))

(defun prep-repl ()
    (rebar_api:debug "Prep'ing REPL ..." '())
    ;; Scan all processes for any with references to the old user and save them
    ;; to update later
    (let ((needs-update (lists:filtermap #'needs-update?/1 (erlang:processes))))
      ;; Start a new shell (this also starts a new user under the correct group)
      (rebar_api:debug "Starting LFE REPL process ..." '())
      (lfe_shell:server)
      ;; Wait until processes have been registered
      (wait-until-user-started 3000)
      ;; Set any process that had a reference to the old user's group leader to
      ;; the new user process. Catch the race condition when the Pid exited
      ;; after the liveness check.
      (catch
        (lists:map #'update-group-leader/1 needs-update))
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
            (rebar_api:debug "Logger changes failed for ~p:~p (~p)"
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
        (erlang:group_leader (whereis 'user) pid))))


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

(defun add-test-paths (state)
  (lists:map #'add-app-test-path/1 (rebar_state:project_apps state))
  (add-cwd-test-path state))

(defun add-app-test-path (app)
  (-> app
      (renar_app_info:out_dir)
      (add-test-path)))

(defun add-cwd-test-path (state)
  (-> state
      (rebar_dir:base_dir)
      (add-test-path)))

(defun add-test-path (path)
  (-> path
      (list "test")
      (filename:join)
      (code:add_path)))

(defun register-agent (pid)
  (rebar_api:debug "Registering rebar_agent ..." '())
  (case (register 'rebar_agent pid)
    ('true
      'ok)
    (_
      (error #(registration-error "Failed to register rebar agent process.")))))
