## Introduction

Erlang is
- functional (immutable) (referential transparency)
- concurrency, high reliability
- actor model, each actor is a separate (lightweight) process in the virtual machine
- communicate by writing messages to mailboxes
- compiled to bytecode and runs on a virtual machine
- "let it crash" policy

> Q: does functional -> immutable ?

## Erlang shell (erl)

- readline shortcuts
- help().
- period required to run commands
- quit().
- ^G to abort shell

## Language

- expressions must be terminated with period
- expressions can be separated with comma, but only result of shown one will be shown
- Erlang was implemented in Prolog, inherits a bit of its syntax
- freely mix int and floating point
- use div or rem for int div and rem
- Base#Value (Base \in 2..26)
- = will compare operands, returning value if same, assign if unbound variable, or raise exception if different
- = does pattern matching essentially
- variables must begin with capital letter
- _ for variables that we don't care about, it won't ever store value
- atoms start with lowercase letters, constants with their own name for value, immutable
- atom can be enclosed in ' if it does not begin with lowercase or contain characters other than alphanumeric, _ or @.
- atom with ' is the same as without '
- atoms are referred to in an atom table, 4 bytes/atom in 32-bit, 8 bytes/atom in 64-bit, not gc-ed, oom when 1048577 atoms
- reserved words cannot be used as atoms
  (after and andalso band begin bnot bor bsl bsr bxor case catch cond div end fun if let not of or orelse query receive rem try when xor)
- booleans, true false, and or xor not
- and or does not short circuit, use andalso orelse
- equality testing, exact =:= =/=, not exact == /= (for int and fp)
- comparison < > >= =<
- comparing diff types may lead to weird results, ordering of types:
  (number < atom < reference < fun < port < pid < tuple < list < bit string)
- tuple {a,b,c}, Point = {4,5}. {X, Y} = Point.
- tagged tuple, a tuple with an atom and an element following it: {celcius, 23.213}
- list can have multiple types: [1, 2, {num, [4, 5]}, atom]
- strings are list of numbers, same notation
- list of numbers printed as string if all of them are chars, printed as list if one of them cannot be represented
- ++ for list append, -- list remove, right associative
- hd, tl of list
- built-in functions (BIF), usually cannot be implemented in pure erlang
- [Hd|Tail] pattern matches or creates lists, | is the cons operator
- list comprehensions: `[2*N || N <- [1,2,3,4]].`
- NewList = [Expression || GeneratorExpr1, ..., Condition1 ...]
- bit syntax `<<Color:24>>`, put Color into 24 bits
- `<<Pix1:24, Pix2:24, Pix3:24>> = Pixels.` to pattern match
- `<<R:8, Rest/binary>> = X.`
- ways to describe binary `<<Value>>` `<<Value:Size>>` `<<Value/TypeSpecifierList>>` `<<Value:Size/TypeSpecifierList>>`
- TypeSpecifierList (- separated) can represent
  types (integer, float ...), signedness (signed, unsigned), endianness, unit (size of each segment in bits)
- bit operators bsl bsr band bor bxor bnot
- binary comprehensions, `[X || <<X>> <= <<1,2,3,4,5>>, X rem 2 == 0]`

## Modules
- functions grouped in a single file under a single name
- modules have functions and attributes
- attributes have the form `-Name(Attribute).`
- compulsory attribute `-module(Name).`
- define exported functions of a module using `-export([Function/Arity,...]).`
- define a function using `name(params) -> body.`
- last logical expression of function is implicitly returned to caller
- single line comments using `%`
- use `-import(Module, [Function/Arity, ...]).` to import modules
- file name is the same as module name defined in `-module`
- define macro using `-define(MACRO, value).`
- use macro using `?MACRO` inside any function
- a function macro defined using `-define(sub(X,Y), X-Y).`
- use function macro using `?sub(1,2).`
- call compiler using `earlc flags file.erl`
- call compiler using `c()` when in shell
- functions must always return something, `io:format` returns the atom `ok`
- exception if function doesn't exist
- common flags: `-debug_info`, `-{outdir,Dir}`, `-export_all`, `-{d,Macro}` or `{d,Macro,Value}`
- can define `-compile([flag...])` in module so that flags are added when module is compiled
- to compile to native code, use `hipe:c`
- `module:module_info/0` or `module:module_info/1` to see metadata of module

## Functions
- pattern-matching function arguments
- function clauses separated by `;`, together they form a function declaration
- guards are additional clauses in functions
- `,` acts like `andalso` and `;` acts like `orelse`
- `,` requires both guards to succeed
- `;` keeps trying until one succeeds
- `if` clauses are called guard patterns, only limited set of functions can be used as guards
- they are not like other `if`, don't compare, pretend this is something different
- everything has to return something, if there are no true clauses, Erlang doesn't know what to return
- `true -> expr` acts like the `else`
- Erlang has no `null`
- can have many guards in if
- `case` is more flexible, can call functions
- `case` is pretty much the same as using multiple functions

## Types
- dynamic type, errors at runtime
- Erlang built on the notion that failure in one component should not affect the entire system
- language includes features that allow distribution of program to different nodes,
  handle unexpected errors, never stop running
- assumes errors will happen and handle them
- strongly typed (no implicit type conversions)
- type conversion functions, `erlang:<type>_to_<type>`
- type test functions, `is_<type>`, returns boolean
- Simon Marlow and Philip Wadler tried building a type system, didn't work well due to lack of process types and inter-process message
- Dialyzer, static analysis toolc:w
- success typings, won't find exact type of every expression, but types found are right, and type errors are really errors
- and will be `and(_,_) -> bool()`, rather than `bool -> bool -> bool`
- use typer / dialyzer
- type and function specifications available in docs

## Recursion
- define function clauses, can call itself
- tail recursion, tall call optimization

## Higher order functions
- functions that can accept functions
- name functions using `fun Module:Function/Arity`
- anonymous functions `fun(Args) -> Exprs;... end`
- can actually be named in and referred in its own scope

## Errors
- raise exceptions, `erlang:error(Reason)`
- exits, won't copy stack trace, error will
- throws are for control flow, programmer expected to handle
- `try .. .catch` to handle exceptions
- in catch branches, if not type specified, defaults to catching `Throw`
- `after` block, similar to `finally`, cannot get any value out of it

## Data structures
- records, `-record(name, {attrs})`, sugar on top of tuples
- in shell, `rr` to load record definitions from modules
- some other functions in shell, `rd` to define, `rf` to unload, `rl` to print
- dot syntax for records
- pattern matching `fn(#rec{a=A, b=B})` and giving names to fields
- header files `.hrl` to export and import records
- proplist, any list of tuples of the form `[{Key,Value}]`
- orddict, ordered dictionaries
- dicts, gb_trees (general balanced trees)
- sets, ordsets, gb_sets, sofs (sets of sets)
- directed graphs, digraph
- queues, double-ended FIFO

## Concurrency
- having many actors run independently, not necessarily at the same time (that would be parallelism)
- scalability, processes started, switched, destroyed very quickly
- forbid memory sharing, communicate by sharing messages, slow but safer (reduce inconsistent states)
- reliability, errors should die as fast as possible to avoid propagating and infecting the rest of the system
- asynchronous message passing
- an Erlang process is ~300 words of memory
- VM starts one thread per core which acts as a scheduler
- each scheduler has a run queue, list of processes to spend time on
- VM will load balance the run queues
- your program only goes as fast as its slowest sequential part, Amdahl's Law
- an Erlang process is basically a function, it runs a function and disappear onces done
- check Erlang VM output `[smp:x:y] [rq:z]`, x - number of cores, y - number of schedulers, z - number of run queues
- `spawn/1` to start a function, returns a pid
- `!` to send message, `<pid> ! <any term>`
- use `self/0` to get current pid
- can send same message to many process by `p1 ! p2 ! msg`, `!` is right associative
- messages go into process' mailbox, kept in order received
- receiving using `receive`, looks like `case`, can have guard too
- `spawn/3` takes module, function, arguments
- receive will wait until get message
- can relay message but only by sending pid (like a return address)
- process will die after receiving message, can call itself recursively

## Multiprocessing
- keep state as function parameters
- pass initial state as parameters to spawn
- helper functions to help send messages
- `?MODULE` is a macro returning the current module's name
- `receive ... after Delay ... end.` is a way to specify timeout
- special case when timeout is 0, until mailbox empty, 'flushing' concept
- build a pid using `pid/3`

## Errors and Processes
- a link is a relationship between two processes
- one one process dies from unexpected throw, error or exit, the linked process also dies
- create a link between current process and another using `link/1`
- to remove a link use `unlink/1`
- no message is sent if process dies of natural causes, return from function
- links are bidirectional
- links cannot be stacked, only 1 link between two processes
- error propagation across processes done through signals
- exit(kill) and killed looks similar but are different, understanding them is key to understanding Erlang's process management

## Monitors
- special type of link, unidirectional and can be stacked
- set up using `erlang:monitor/2`
- use `register/2` to give a name to a process
- use `whereis/1` to find pid of a named process
- can use `make_ref` to create unique references, used for identifying messages

## Concurrent application
- Erlang has a code server, a process in charge of an ETS table
- code server can hold two versions of a single module in memory, both versions can run at once
- when there are two versions, local calls are down on the currently running version
- external calls (via module) done on newest version of code
- code upgrade via `Module:Upgrade(CurrentState)`

## OTP
- abstraction of common processes, spawn, init, loop exit

## Clients and servers
- implement a few functions `gen_server` needs, call some functions in `gen_server`
- `init` to start server
- `handle_call` for synchronous messages
- `handle_cast` for asynchronous messages
- `terminate` called when stop
- `behaviour`, functions that a module should have (contract)

## Finite State Machines

## Generic Event Handlers

## OTP applications
- directory structure, way to handle config, env vars, start/stop apps, etc
- <myapp>.app

## Distributed
- a VM that is ready to be connected is called a node
- each VM is a node
- you can have 50 nodes on a single computer, or 50 nodes in 50 computers, doesn't matter
- Erlang Port Mapper Daemon (EPMD), runs on each of the computers which are part of the Erlang cluster
- EPMD acts as a name server that lets nodes registers themselves, contact other nodes
- when a node connects to another, they automatically start monitoring one another
- when a new node joins another node which is already part of a group of nodes, the new node gets connected to the entire group
