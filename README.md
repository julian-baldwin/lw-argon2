# Argon2 for LispWorks

A lightweight high-level binding to the Argon2 reference implementation, with an embedded FLI
module. This is spun off my own internal work as Ironclad doesn't implement the ID variant or
parallelism.

## License

### The MIT License

> Copyright (c) 2022 Julian Baldwin.
>
> Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
> associated documentation files (the "Software"), to deal in the Software without restriction,
> including without limitation the rights to use, copy, modify, merge, publish, distribute,
> sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
> furnished to do so, subject to the following conditions:
>
> The above copyright notice and this permission notice shall be included in all copies or
> substantial portions of the Software.
>
> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
> NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
> NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
> DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
> OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

## Platform Support

MacOS (and by extension Linux) should work out of the box. I do not have Windows/FreeBSD
environments. There are two places in particular that require addressing: the compiler flags used to
compile the embedded library (see `embed.lisp`) and the function `ARGON2::RANDOM-DATA` in
`argon2.lisp` which is used to generate a random salt. On the other platforms this is done by
reading from `/dev/urandom` - probably the easiest solution on Windows is to use `RtlGenRandom` or
an equivalent function, but I can't test this so haven't implemented it. I can't justify making it
dependent on Ironclad or similar just for a single function, however you could do this if desired.

## Usage

LW-Argon2 includes the [Argon2 reference implementation](https://github.com/P-H-C/phc-winner-argon2)
as a Git submodule. To embed the Argon2 library, initialise this submodule and and arrange to call
`ARGON2:INITIALISE-EMBEDDED` at runtime. You are responsible for ensuring you comply with its (very
liberal) license. Embedding works by concatenating the relevant source files from the reference
implementation and creating a LispWorks system with this concatenated source as an embedded module.
This is compiled at load time with a few tweaks to the compiler flags for optimisation and include
paths.

Example:

    CL-USER 1 > (ql:quickload "lw-argon2")
    ;; output elided
    CL-USER 2 > (argon2:initialise-embedded)
    CL-USER 3 > (argon2:hash-password "test")
    "$argon2id$v=19$m=4096,t=3,p=1$TE2DcVMJrRx098254AeMmg$cZdDIaW3tpYrLnx8mnyT/AWT3d0Sb1h3sm43HJJlNmY"
    CL-USER 4 > (argon2:verify-password "test" *)
    T

Use the function `ARGON2:HASH-PASSWORD` to generate an Argon2 hash of a password. There are keyword
arguments for the time, memory and parallelism parameters and the type of Argon2 hash (I/D/ID). The
defaults are available as specials in order to configure them for your use case. I have used the
default values for the argon2 command line program as initial values. You may elect to generate and
supply your own salt using the `:SALT` keyword. If this is nil, a random salt will be generated (see
above for caveats). The type of hash should be one of the symbols `ARGON2:ARGON2-I`,
`ARGON2:ARGON2-D` or `ARGON2:ARGON2-ID`. Passwords are encoded using the default `:EF-MB-STRING`
encoding without a null terminator.

Use the function `ARGON2:VERIFY-PASSWORD` to verify a password against an encoded hash. By default
this function will signal `ARGON2:INCORRECT-PASSWORD` on failure, however you can disable this via
keyword argument. It will always signal `ARGON2:ARGON2-ERROR` on other kinds of failure such as an
invalid hash.

The function `ARGON2:VERIFY-NO-LOGIN` is available to perform a dummy verification to resist timing
attacks aimed at detecting user accounts. By default it signals an incorrect password, but this can
also be disabled by keyword argument.

## Tests

The test package contains some sample test vectors taken from the reference implementation C source
as a basic sanity/smoke test. It does use [Parachute
Browser](https://github.com/julian-baldwin/parachute-browser) to run from the IDE.

Testing and/or patches for other platforms would be gratefully accepted.
