def getOp (opcode x y : Nat) : Nat :=
	match opcode with
	| 1 => x + y
	| 2 => x * y
	| _ => 0

partial def runProgram' (p : Array Nat) (i : Nat) : Array Nat :=
	if p[i] == 99 then
		p
	else
		let opcode := p[i]
		let arg1 := p[p[i+1]]
		let arg2 := p[p[i+2]]
		let dst := p[(i+3)]
		let p := p.set! dst (getOp opcode arg1 arg2)
		runProgram' p (i + 4)

def runProgram (p : Array Nat) (noun verb : Nat) : Nat :=
	let p := p.set! 1 noun
	let p := p.set! 2 verb
	let p := runProgram' p 0
	p[0]

def findInputs (p : Array Nat) : IO Unit := do
	for noun in [0:100] do
		for verb in [0:100] do
			if runProgram p noun verb == 19690720 then
				IO.println s!"noun = {noun}"
				IO.println s!"verb = {verb}"
				IO.println s!"100 * noun + verb = {100 * noun + verb}"

def main : IO Unit := do
	let file <- IO.FS.readFile "02-input.txt"
	let p := (file.splitOn ",").map String.toNat!
	findInputs p.toArray
