// Usage: node bf 
// BF interpreter

'use strict';

var fs = require('fs');

function main() {
  let argv = process.argv.slice(2)
  if (argv.length < 1) {
    throw "bad usage - no program file specified"
  }
  let progfile = argv[0]
  let memsize = parseInt(argv[1]) || 10000
  let input = argv[2] || ''

  var prog = fs.readFileSync(progfile, "utf8");
  new BF(memsize, prog, input).run()
}

function compute_jumps(program) {
  let jumps = Array(program.length).fill(0)
  let stack = []
  for (let i = 0; i < program.length; ++i) {
    if (program[i] == '[') {
        stack.push(i)
    } else if (program[i] == ']') {
      if (stack.length > 0) {
        let x = stack.pop()
        jumps[i] = x+1
        jumps[x] = i+1
      } else {
        throw "bad [] pairing"
      }
    }
  }
  return jumps
}

class BF {
  constructor(memsize, program, input) {
    this.memsize = memsize
    this.memory = Array(memsize).fill(0)
    this.program = program + "\0" // a string
    this.input = input

    this.jumps = compute_jumps(program)
    this.dp = 0
    this.pc = 0
    this.inp = 0
    this.halted = 0
  }

  run() {
    while (!this.halted) { this.step() }
  }

  step() {
    let ch = this.program[this.pc]
    switch (ch) {
      case '!':
        { let msg = ''
          while (1) {
            let x = this.program[++this.pc]
            if (x == '!') break
            if (x != '\n') msg += x
          }
          this.pc++
          process.stdout.write("DEBUG " + msg + " at " + this.dp + " = " + this.memory[this.dp]+"\n")
        }
        break
      case '\0':
        this.halted = 1
        break
      case '>':
        this.dp++
        if (this.dp >= this.memsize) {
          throw ("memory fault: " + this.dp)
        }
        this.pc++
        break
      case '<':
        this.dp--
        if (this.dp < 0) {
          throw ("memory fault: " + this.dp)
        }
        this.pc++
        break
      case '+':
        { let x = ++this.memory[this.dp]
          if (x > 255) this.memory[this.dp] = 0
          this.pc++
        }
        break
      case '-':
        { let x = --this.memory[this.dp]
          if (x < 0) this.memory[this.dp] = 255
          this.pc++
        }
          break
      case '[':
        if (this.memory[ this.dp ]) {
          this.pc++;
        } else {
          this.pc = this.jumps[ this.pc ];
        }
        break
      case ']':
        if (this.memory[ this.dp ]) {
          this.pc = this.jumps[ this.pc ];
        } else {
          this.pc++;
        }
        break
      case '.':
        { let ch = this.memory[this.dp]
          process.stdout.write(String.fromCharCode(ch))
          // console.log("output: " + ch + " " + String.fromCharCode(ch))
        }
        this.pc++
        break
      case ',':
        if (this.inp < this.input.length) {
          this.memory[ this.dp ] = this.input.charCodeAt(this.inp++)
        } else {
          this.memory[ this.dp ] = 0
        }
        this.pc++
        break
      case '@':
        { let ch = this.memory[this.dp]
          console.log("byte at " + this.dp + ": " + ch)
        }
        this.pc++
        break
      default:
        this.pc++
        break
    }
  }
}

main()

