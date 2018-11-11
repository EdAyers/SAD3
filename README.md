## SAD3 ##

*System for Automated Deduction* (SAD 3rd generation) -- Proof Checking of Natural Mathematical Documents.

SAD3 is a different approach to a proof assistant.
In SAD, the user writes their mathematical documents in a controlled natural language called ForTheL. ForTheL is designed to capture a lot of the linguistic constructions that would be found in a natural language textbook in mathematics.
The ForTheL text is then parsed in to FOL and then this is fed in to any number of automated theorem proving tools such as E, SPASS and Vampire through the TPTP language. Providing a formal check of the mathematical document's correctness.

SAD does not include a proof checking kernel. If there is a bug in the correctness of one of the provers, SAD will report a false result as correct.

The philosophy of SAD is very much that the user should be able to pick up and start using a proof assistant without first familiarising themselves with an elaborate logic or language. They should be able to write the mathematics they are used to and it 'just works'. While SAD is still a long way from this vision it is a great step forward.

# Papers #

- [_Paskevich_ -The Syntax and Semantics of the ForTheL Language](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.211.8865&rep=rep1&type=pdf)
- [TODO]

# Build #

* Isabelle:
      ```
      isabelle build -D Isabelle
      ```

* Haskell:
      ```
      stack build
      ```
## Setting up `eprover`.

If you run `stack exec SAD3-exe -- "examples/Koenigs_lemma.ftl"` now, you will get the error `[Export] failed to run "~/forksE/PROVER/eprover": does not exist`.
SAD3 is able to use a number of provers to prove the first order logic theorems that it generates. By default, SAD3 is set up to use the [E Prover](https://wwwlehre.dhbw-stuttgart.de/~sschulz/E/Download.html).
Put a copy of the `eprover` executable in the `provers` folder of this project.

### Building the E Prover from source
The E prover website is not that helpful for quickly getting a copy of `eprover`. 
So you can run the below commands to build it from source which worked in November 2018.
```sh
# do this in the SAD folder (or wherever you want)
cd provers
wget http://wwwlehre.dhbw-stuttgart.de/~sschulz/WORK/E_DOWNLOAD/V_2.2/E.tgz
tar zxvf E.tgz
rm E.tgz
cd E
# Now following the build instructions in E's README
./configure
make
cd ..
# Copy over the executable to SAD3/provers directory.
cp E/PROVER/eprover .
rm -rf E
cd ..
```

# Test
```sh
for FILE in examples/*.ftl
do
stack exec SAD3-exe -- "$FILE"
done
```

# Isabelle PIDE #

* edit $ISABELLE_HOME_USER/etc/settings to include this directory as component, e.g.:

      init_component "$HOME/isabelle/SAD3/repos"

* open theory with Isabelle/jEdit, e.g.

      isabelle jedit -l Pure Isabelle/Test.thy


# Development #


## [Haskell Stack](https://www.haskellstack.org)

On macOS do: `brew install stack`.

## [Haskell IDE](https://github.com/haskell/haskell-ide-engine) within VSCode 
If you want to tinker with the Haskell code, we reccomend using VSCode with the following setup:
Build HIE from source:
```sh
cd ~
git clone https://github.com/haskell/haskell-ide-engine.git
cd haskell-ide-engine
stack build
```
Then install the [VSCode Haskell Language Server Extension](https://marketplace.visualstudio.com/items?itemName=alanz.vscode-hie-server).
Then add this line to your `settings.json`:
```json
  "languageServerHaskell.hieExecutablePath": "~/.local/bin/hie",
```
If you want to see documentation on hover then run `stack haddock --keep-going` in your project directory.

# Options

The SAD3 command line tool takes the following options:

- `-T` show the FOL output.
- `-v`, `-vv`, `-vvvvvv`, varying degrees of verbosity.
- [TODO]

# Tips

### Switching off the prover.

You can force SAD to accept theorems without trying to prove them:
```forthel
[prove off]
Theorem not_proven. x != x.
[/prove]
Theorem proven. x = x.
```
This saves lots of time because you don't have to wait for stuff that was proven last time to be proven.
The plan is to have a continuous proof-checking feature soon.
<!-- 
### VSCode language support for ForThel

Ed Ayers has written a prototype vscode extension for ForTheL with syntax colouring and simple error diagnostics. https://github.com/EdAyers/vscode-forthel -->

