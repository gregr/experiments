function toZeroCPSAlloc(count) {
  if (count === 0) {
    return ['done', null];
  }
  return [toZeroCPSAlloc, count - 1];
}

function toZeroCPS(toZeroCPSResult, count) {
  if (count !== 0) {
    toZeroCPSResult[0] = toZeroCPS;
    toZeroCPSResult[1] = count - 1;
    return;
  }
  toZeroCPSResult[0] = 'done';
  toZeroCPSResult[1] = null;
}

// ~160ms
function benchTrampolineAlloc(count) {
  var cont = toZeroCPSAlloc;
  var arg = count;
  while (arg !== null) {
    var result = cont(arg);
    cont = result[0];
    arg = result[1];
  }
  return cont;
}

// ~40ms
function benchTrampoline(count) {
  var toZeroCPSResult = new Array(2)
  var cont = toZeroCPS;
  var args = count;
  while (args !== null) {
    cont(toZeroCPSResult, args);
    cont = toZeroCPSResult[0];
    args = toZeroCPSResult[1];
  }
  return cont;
}

// ~11ms
function benchLoop(count) {
  while (count > 0) {
    count = count - 1;
  }
  return 'done';
}

// ~40ms
function benchLoopNop1(count) {
  var toZeroCPSResult = new Array(2)
  var cont = toZeroCPS;
  var args = 10;
  while (count > 0) {
    toZeroCPS(toZeroCPSResult, args);
    cont = toZeroCPSResult[0];
    args = toZeroCPSResult[1] + 1;
    args = 10;
    count = count - 1;
  }
  return 'done';
}

// ~110ms
function benchLoopNop2(count) {
  var irrelevant = [undefined];
  irrelevant.pop();
  var toZeroCPSResult = new Array(2)
  var cont = toZeroCPS;
  var args = 10;
  while (count > 0) {
    irrelevant.push(1);
    irrelevant.pop();
    toZeroCPS(toZeroCPSResult, args);
    cont = toZeroCPSResult[0];
    args = toZeroCPSResult[1] + 1;
    args = 10;
    count = count - 1;
  }
  return 'done';
}

function timed(f) {
  var start = Date.now();
  var result = f();
  var elapsed = Date.now() - start;
  return [result, elapsed];
}

// ~160ms
console.log(timed(function() { return benchTrampolineAlloc(10000000); }))
// ~11ms
console.log(timed(function() { return benchLoop(10000000); }))
// ~40ms
console.log(timed(function() { return benchLoopNop1(10000000); }))
// ~110ms
console.log(timed(function() { return benchLoopNop2(10000000); }))
// ~40ms
console.log(timed(function() { return benchTrampoline(10000000); }))
