# lfe-repl

*The LFE rebar3 REPL plugin*

[lr3-logo]: priv/images/logo.png

[![][lr3-logo]][lr3-logo]


#### Contents

* [Build](#build-)
* [Use](#use-)


## Build [&#x219F;](#contents)


```bash
$ rebar3 compile
```


## Use [&#x219F;](#contents)

Add the required plugins and provider hooks to your ``rebar.config``:

```erlang
{plugins, [
  {'lfe-compile',
    {git, "https://github.com/lfe-rebar3/compile.git", {tag, "0.3.0"}}},
  {'lfe-repl',
    {git, "https://github.com/lfe-rebar3/repl.git", {tag, "0.2.0"}}}
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
