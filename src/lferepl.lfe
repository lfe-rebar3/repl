;;;; This plugin was translated from the original Erlang rebar3 plugin for
;;;; starting up an Erlang shell:
;;;;    https://github.com/rebar/rebar3/blob/6637ec45481b9ae37a3ed84c264eb0dbbe76628d/src/rebar_prv_shell.erl
;;;;
(defmodule lferepl
  (behaviour provider)
  (export all))

(defun namespace () 'lfe)                   ; All LFE plugins need to have this
(defun provider-name () 'repl)
(defun short-desc () "The LFE rebar3 LFE REPL plugin.")
(defun deps () '())

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Public API
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun init (state)
  (let* ((opts `(#(name ,(provider-name))        ; The 'user friendly' name
                 #(module ,(MODULE))             ; The module implementation
                 #(namespace ,(namespace))       ; Plugin namespace
                 #(deps ,(deps))                 ; The list of dependencies
                 #(example "rebar3 lfe repl")    ; How to use the plugin
                 #(short_desc ,(short-desc))     ; A one-line description
                 #(desc ,(info (short-desc)))    ; A longer description
                 #(bare true)                    ; The task can be run by user
                 #(opts (#(config undefined "config" string
                           (++ "Path to the config file to use. Defaults to the "
                               "sys_config defined for relx, if present."))
                         #(name undefined "name" atom
                           "Gives a long name to the node.")
                         #(sname undefined "sname" atom
                           "Gives a short name to the node.")))))
         (provider (providers:create opts)))
    `#(ok ,(rebar_state:add_provider state provider))))

(defun do (state)
  (try
    (progn
  (lfe-repl-setup:set-name state)
  (lfe-repl-setup:set-paths state)
  (lfe-repl-setup:prep-repl)
  (lfe-repl-app:boot-apps state)
  (lfe-repl-setup:simulate-proc-lib)
  )
    (catch (err (progn (io:format "Exception: ~p~n" `(,err))
      (error err)))))
  (case (register 'rebar_agent (self))
    ('true 'ok)
    (_ (error #(registration-error "Failed to register rebar agent process."))))
  (case (rebar_agent:init state)
    (`#(ok ,gen-state)
      (gen_server:enter_loop 'rebar_agent
                             '()
                             gen-state
                             #(local rebar_agent)
                             'hibernate))
    (_ (error #(init-error "Failed to initialize rebar agent state."))))
  `#(ok ,state))

(defun format_error (reason)
  (io_lib:format "~p" `(,reason)))

;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Internal functions
;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun info (desc)
  (io_lib:format
    (++ "~n~s~n~n"
        "Start an LFE REPL for a project with its dependencies preloaded, "
        "similar to 'lfe -pa ebin -pa deps/*/ebin' with support for -name "
        "and -sname parameters."
        "~n")
    `(,desc)))
