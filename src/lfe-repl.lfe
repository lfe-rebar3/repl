;;;; This plugin was translated from the original Erlang rebar3 plugin for
;;;; starting up an Erlang shell:
;;;;    https://github.com/rebar/rebar3/blob/6637ec45481b9ae37a3ed84c264eb0dbbe76628d/src/rebar_prv_shell.erl
;;;;
(defmodule lfe-repl
  (behaviour provider)
  (export all))

(defun namespace () 'lfe)
(defun provider-name () 'repl)
(defun short-desc () "The LFE rebar3 LFE REPL plugin.")
(defun deps () '(#(default lock)))
(defun task-options ()
  `(#(name undefined "name" atom "Gives a long name to the node.")
    #(sname undefined "sname" atom "Gives a short name to the node.")
    #(apps undefined "apps" atom
      ,(++ "A list of apps to boot before starting the shell. (E.g. --apps "
           "app1,app2,app3) Defaults to rebar.config {shell, [{apps, Apps}]} "
           "or relx apps if not specified."))))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Public API
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun init (state)
  (init state (MODULE)))

(defun init (state module)
  (let* ((opts `(#(name ,(provider-name))        ; The 'user friendly' name
                 #(module ,module)               ; The module implementation
                 #(namespace ,(namespace))       ; Plugin namespace
                 #(deps ,(deps))                 ; The list of dependencies
                 #(example "rebar3 lfe repl")    ; How to use the plugin
                 #(short_desc ,(short-desc))     ; A one-line description
                 #(desc ,(info (short-desc)))    ; A longer description
                 #(bare true)                    ; The task can be run by user
                 #(opts ,(task-options))))
         (provider (providers:create opts)))
    `#(ok ,(rebar_state:add_provider state provider))))

(defun do (state)
  (lr3-repl-setup:set-name state)
  (lr3-repl-setup:set-paths state)
  (lr3-repl-setup:prep-repl)
  (lr3-repl-app:boot-check state)
  (lr3-repl-setup:simulate-proc-lib)
  (lr3-repl-setup:register-agent (self))
  (rebar_api:debug "Initializing rebar_agent ..." '())
  (case (rebar_agent:init state)
    (`#(ok ,gen-state)
      (rebar_api:debug "Starting rebar_agent event loop ..." '())
      (gen_server:enter_loop 'rebar_agent
                             '()
                             gen-state
                             #(local rebar_agent)
                             'hibernate))
    (_
      (let ((msg "Failed to initialize rebar agent state."))
        (rebar_api:error msg '())
        (error #(init-error msg)))))
  `#(ok ,state))

(defun format_error (reason)
  (io_lib:format "~p" `(,reason)))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Internal functions
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun info (short-desc)
  (io_lib:format
    (++ "~n~s~n~n"
        "Start an LFE REPL for a project with its dependencies preloaded, "
        "similar to~n"
        "'lfe -pa ebin -pa deps/*/ebin' with support for -name "
        "and -sname parameters."
        "~n")
    `(,short-desc)))
