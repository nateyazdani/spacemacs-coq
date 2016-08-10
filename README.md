# Coq Spacemacs layer

Adds support for Coq via [Proof General](https://github.com/ProofGeneral/PG) and [company-coq](https://github.com/cpitclaudel/company-coq).

## Installation

To install, run `git clone https://github.com/tchajed/spacemacs-coq ~/.emacs.d/private/coq` and add `coq` to your `dotspacemacs-configuration-layers` list.

## Shortcuts

This layer adds Spacemacs-style shortcuts for the most useful parts of Proof General and company-coq's functionality.

### Proof management

Key Binding  | Description
-----------  | -----------
`<f4>`, `,.` | Go to point
`<f3>`, `,]` | Process next command
`<f2>`, `,[` | Undo previous command

### Laying out windows

Key Binding  | Description
-----------  | -----------
`,ll`        | Re-layout windows
`,lc`        | Clear response buffer
`,lp`        | Show current proof

### Managing prover process

Key Binding  | Description
-----------  | -----------
`,pc`        | Interrupt prover
`,px`        | Quit prover
`,pr`        | Retract buffer

### Prover queries

The mnemonic for `a` is "ask".

Key Binding  | Description
-----------  | -----------
`,af`        | Search (mnemonic: "find theorems")
`,ap`        | Print
`,ac`        | Check
`,ab`        | About
`,aip`       | Print (showing implicits)
`,aic`       | Check (showing implicits)
`,aib`       | About (showing implicits)
`,anp`       | Print (showing all; mnemonic for `n` is "notations")
`,anc`       | Check (showing all; mnemonic for `n` is "notations")
`,anb`       | About (showing all; mnemonic for `n` is "notations")

### Moving the point

Key Binding  | Description
-----------  | -----------
`,g.`         | Go to last processed command
`,ga`         | Go to start of command at point
`,ge`         | Go to end of command at point

### Inserting

Key Binding  | Description
-----------  | -----------
`,il`        | Extract lemma from current goal - exit with `C-RET` (not `C-j`)
`,im`        | Insert `match` on a type
`M-RET`      | Insert regular match branch
`M-S-RET`    | Insert `match goal with` branch

Note the last two are regular company-coq bindings, left alone since they are most useful in insert mode.

## Note about cursor color

This package forces the insert mode cursor color to match normal mode, to avoid a serious performance problem in Evil where proof navigation in insert mode is extremely slow compared to the same commands in normal mode. See https://github.com/olivierverdier/spacemacs-coq/issues/6 for details.
