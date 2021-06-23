def restore (p : Array Nat) (noun verb : Nat) : Array Nat :=
	let p := p.set! 1 noun
	let p := p.set! 2 verb
	p

def getOp : Nat -> Nat -> Nat -> Nat := fun
	| 1 => (. + .)
	| 2 => (. * .)
	| _ => panic! "invalid opcode"

partial def runProgram (p : Array Nat) (i : Nat) : Nat :=
	if p[i] == 99 then
		p[0]
	else
		let opcode := p[i]
		let arg1 := p[p[i+1]]
		let arg2 := p[p[i+2]]
		let dst := p[(i+3)]
		let p := p.set! dst (getOp opcode arg1 arg2)
		runProgram p (i + 4)

def findInputs (p : Array Nat) : IO Unit := do
	for noun in [0:100] do
		for verb in [0:100] do
			let result := runProgram (restore p noun verb) 0
			if result == 19690720 then
				IO.println s!"noun = {noun}"
				IO.println s!"verb = {verb}"
				IO.println s!"100 * noun + verb = {100 * noun + verb}"
				return

def main : IO Unit := do
	let file <- IO.FS.readFile "02-input.txt"
	let p := (file.trim.splitOn ",").map String.toNat!
	findInputs p.toArray
