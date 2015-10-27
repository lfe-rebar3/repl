(defmodule lfe-repl-cfg
  (export all))

(include-lib "clj/include/compose.lfe")

(defun reread (state)
  (case (find state)
    ('noconfig 'ok)
    (config-list
      (lists:map #'update-env/1 config-list))))

(defun update-env
  ((`#(,app #(,key ,val)))
    (application:set_env app key val)))

(defun find (state)
  (case (find-option state)
    ('no_config
      (find-relx state))
    (result result)))

(defun find-option (state)
  (let ((`#(,opts ,_) (rebar_state:command_parsed_args state)))
    (case (proplists:get_value 'config opts)
      ('undefined
        'no_config)
      (filename
        (consult-cfg state filename)))))

(defun find-relx (state)
  (let ((opts (rebar_state:get state 'relx '())))
    (case (proplists:get_value 'sys_config opts)
      ('undefined
        'no_config)
      (filename
        (consult-cfg state filename)))))

(defun consult-cfg (state filename)
  (let ((full-path (-> (rebar_dir:root_dir state)
                       (list filename)
                       (filename:join))))
    (rebar_api:debug "Loading configuration from ~p" `(,full-path))
    (case (rebar_file_utils:try_consult full-path)
      (`(,result)
        result)
      ('()
        '()))))

