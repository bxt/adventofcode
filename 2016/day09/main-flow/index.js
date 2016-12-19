// @flow

const fs = require('fs');
const assert = require('assert');

function expandedLenght(input, recursive) {
  const factors = [];

  let result = 0;
  let factor = 1;
  let until = 0;
  let lastEnd = 0;

  input.replace(/\((\d+)x(\d+)\)/g, function(exp, length, times, at){
    if (at >= until) {
      result += (at - until)
      result += (until - lastEnd) * factor

      lastEnd = at + exp.length
      until = lastEnd + 1*length
      factor = 1*times
    } else {
      // nothing?
    }
    return ''
  })

  result += (input.length - until)
  result += (until - lastEnd) * factor

  return result
}

assert.equal(expandedLenght("ADVENT"), 6)
assert.equal(expandedLenght("A(1x5)BC"), 7)
assert.equal(expandedLenght("(3x3)XYZ"), 9)
assert.equal(expandedLenght("A(2x2)BCD(2x2)EFG"), 11)
assert.equal(expandedLenght("(6x1)(1x3)A"), 6)
assert.equal(expandedLenght("X(8x2)(3x3)ABCY"), 18)

const input = fs.readFileSync('../input.txt', 'utf-8').trim()

console.log(expandedLenght(input))
