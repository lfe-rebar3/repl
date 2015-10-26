(defmodule lfe-repl-app
  (export all))

(defmacro ->
  ((x) x)
  ((x sexp) (when (is_list sexp))
   `(,(car sexp) ,x ,@(cdr sexp)))
  ((x sexp)
   `(list ,sexp ,x))
  ((x sexp . sexps)
   `(-> (-> ,x ,sexp) ,@sexps)))

(defmacro ->>
  ((x) x)
  ((x sexp) (when (is_list sexp))
   `(,(car sexp) ,@(cdr sexp) ,x))
  ((x sexp)
   `(list ,sexp ,x))
  ((x sexp . sexps)
   `(->> (->> ,x ,sexp) ,@sexps)))


(defun boot-existing-apps (state)
  (case (find-bootable-apps state)
    ('undefined
      ;; try to read in sys.config file
      (lfe-repl-cfg:reread state))
    (apps
      ;; load apps, then check config, then boot them
      (load-apps apps)
      (lfe-repl-cfg:reread state)
      (boot-apps apps))))


(defun find-bootable-apps (state)
  ;; Try the shell_apps option
  (case (rebar_state:get state 'shell_apps 'undefined)
    ('undefined
      ;; Get to the relx tuple instead
      (case (lists:keyfind 'release 1 (rebar_state:get state 'relx '()))
        (`#(,_ ,_ ,apps)
          apps)
        ('false
          'undefined)))
    (apps
      apps)))

(defun load-apps (apps)
  (let ((loaded (application:loaded_applications)))
    (->> apps
        (load-apps-normalized)
        (lists:filtermap (lambda (app) (filter-loaded app loaded)))
        (lists:map #'load-app/1))))

(defun filter-loaded (app loaded)
  (case (not (lists:keymember app 1 loaded))
    ('true `#(true ,app))
    ('false 'false)))

(defun load-app (app)
  (case (application:load app)
    ('ok
      (->> app
           (application:get_all_key)
           (proplists:get_value 'applications)
           (load-apps)))
    ;; This will be caught when starting the app in the event of an error.
    (_
      'error)))

(defun boot-apps (apps)
  (rebar_api:warn (++ "The rebar3 shell is a development tool; to deploy "
                      "applications in production, consider using releases "
                      "(http://www.rebar3.org/v3.0/docs/releases)")
                  '())
  (->> apps
       (load-apps-normalized)
       (lists:map #'application:ensure_all_started/1)
       (lists:foreach #'report-boot-status/1))
  'ok)

(defun report-boot-status
  ((`#(ok ,booted))
    (rebar_api:info "Booted ~p" `(,booted)))
  ((`#(error #(,app ,reason)))
    (rebar_api:error "Failed to boot ~p: ~p" `(,app ,reason))))

(defun load-apps-normalized
  (('())
    '())
  ((`(#(,app ,_) . ,tail))
    (cons app (load-apps-normalized tail)))
  ((`(#(,app ,_version load) . ,tail))
    (cons app (load-apps-normalized tail)))
  ((`(,app . ,tail)) (when (is_atom app))
    (cons app (load-apps-normalized tail))))

(defun boot-apps-normalized
  (('())
    '())
  ((`(#(,_app load) . ,tail))
    (boot-apps-normalized tail))
  ((`(#(,_app ,_version load) . ,tail))
    (boot-apps-normalized tail))
  ((`(#(,app ,_version) . ,tail))
    (cons app (boot-apps-normalized tail)))
  ((`(,app . ,tail)) (when (is_atom app))
    (cons app (boot-apps-normalized tail))))

