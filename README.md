# Spacemacs layer for the Coq proof assistant

A spacemacs layer providing Coq support by wrapping [Company Coq](https://github.com/cpitclaudel/company-coq), which builds upon [Proof General](https://github.com/ProofGeneral/PG).

This spacemacs layer derives from the official layer under development, which in turn derives from [Olivier Verdier's layer](https://github.com/olivierverdier/spacemacs-coq) via [Tej Chajed's derivative](https://github.com/tchajed/spacemacs-coq). The biggest changes are to pull Proof General off MELPA through the `package.el` system (rather than expecting manual installation) and to remap the keybindings to my own preference.

## Shortcuts

This layer adds Spacemacs-style shortcuts for the most heavily used functionality of Company Coq and Proof General

### Interacting with the proof

Key Binding  | Description
-----------  | -----------
`,.`         | Go to point
`,]`         | Process next command
`,[`         | Undo previous command
`,=`         | Evaluate selected term

### Laying out windows

Key Binding  | Description
-----------  | -----------
`,ll`        | Re-layout windows
`,lt`        | Toggle proof tree
`,lr`        | Rotate output displays
`,lc`        | Clear response buffer
`,lp`        | Refresh current proof

### Managing the prover process

Key Binding  | Description
-----------  | -----------
`,pc`        | List current proof context
`,ps`        | Toggle active scripting
`,pb`        | Process buffer through to end
`,pr`        | Retract buffer back to start
`,pk`        | Interrupt prover
`,pq`        | Quit prover

### Searching the proof context

Key Binding  | Description
-----------  | -----------
`,sa`        | Search constants by identifier/type fragment(s)
`,sp`        | Search constants by type pattern
`,sr`        | Search equality lemma for rewriting
`,sl`        | Search qualified location of identifier
`,sn`        | Search expanded template of notation

### Inspecting the proof context

Key Binding  | Description
-----------  | -----------
`,ip`        | Print body (optionally showing implicits)
`,iP`        | Print body in full
`,ic`, `,c`  | Check type (optionally showing implicits)
`,iC`        | Check type in full
`,ia`, `,a`  | About item (optionally showing implicits)
`,iA`        | About item in full
`,i!`        | Toggle showing implicit subterms

### Moving the point

Key Binding  | Description
-----------  | -----------
`,g.`        | Go to last processed command
`,g0`        | Go to start of command at point
`,g$`        | Go to end of command at point
`,gb`        | Go to previous command
`,gw`        | Go to next command

### Manipulating the program text

Key Binding  | Description
-----------  | -----------
`,x-`        | Fold body of current proof or definition
`,x+`        | Unfold body of current proof or definition
`,x*`        | Peek definition of symbol at point
`,x/`        | Grep throughout Coq sources in current directory tree
`,xo`        | Show outline of current proof script
`,xg`        | Go to position at point in proof outline
`,xd`        | Compare unification errors or goal states
`,xj`        | Jump to definition of identifier at point

### Retrieving documentation

Key Binding  | Description
-----------  | -----------
`,d`         | View documentation of an identifier

### Finding help

Key Binding  | Description
-----------  | -----------
`,?`         | Show help for a Coq error message
