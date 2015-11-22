# lfe-repl

*The LFE rebar3 REPL plugin*

[lr3-logo]: resources/images/logo.png

[![][lr3-logo]][lr3-logo]


#### Contents

* [Build](#build-)
* [Use](#use-)


## Build


```bash
$ rebar3 compile
```


## Use

Add the required plugins and provider hooks to your ``rebar.config``:

```erlang
{plugins, [
  {'lfe-compile', ".*",
    {git, "https://github.com/lfe-rebar3/compile.git", {tag, "0.2.1"}}},
  {'lfe-version', ".*",
    {git, "https://github.com/lfe-rebar3/repl.git", {tag, "0.1.1"}}}
]}.

{provider_hooks, [
   {pre, [{compile, {lfe, compile}}]}
  ]}.
```

Then just call your plugin directly from your project directory:

```bash
$ rebar3 help lfe repl

The LFE rebar3 LFE REPL plugin.

Start an LFE REPL for a project with its dependencies preloaded, similar to
'lfe -pa ebin -pa deps/*/ebin' with support for -name and -sname parameters.
```

```bash
$ rebar3 lfe repl

LFE Shell V7.0 (abort with ^G)
>
```
