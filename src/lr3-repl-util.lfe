(defmodule lr3-repl-util
  (export all))

(include-lib "clj/include/compose.lfe")

(defun get-apps (state)
  (rebar_api:debug "\tGetting apps ..." '())
  (case (rebar_state:current_app state)
    ('undefined
      (let ((apps (rebar_state:project_apps state)))
        (rebar_api:debug "\tState's current app undefined; using ~p" `(,apps))
        apps))
    (app
      (rebar_api:debug "\tGot app ~s" `(,app))
      (list app))))

(defun get-base-dirs (state)
  (lists:append
    (list (get-base-dir state))
    (get-apps-base-dirs (get-apps state))))

(defun get-base-dir (state)
  (let ((base-dir (rebar_dir:root_dir state)))
    (rebar_api:debug "\tGot project base dir ~p" `(,base-dir))
    base-dir))

(defun get-apps-base-dirs (apps)
  (lists:map #'rebar_app_info:out_dir/1 apps))

(defun get-test-paths (state)
  (->> state
      (get-base-dirs)
      (lists:map (lambda (x) (filename:join x "test")))))
