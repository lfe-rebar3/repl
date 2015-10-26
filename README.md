# lfe-repl

*The LFE rebar3 REPL plugin*

<img src="resources/images/logo.png" />


## Build


```bash
    $ rebar3 compile
```


## Use

Add the plugin to your ``rebar.config``:

```erlang
{plugins, [
  {'lfe-repl', ".*",
    {git, "https://github.com/lfe-rebar3/repl.git",
      {branch, "master"}}}
]}.
```

Then just call your plugin directly from your project directory:

```bash
$ rebar3 help lfe repl
...
```

```bash
$ rebar3 lfe repl
...
```
