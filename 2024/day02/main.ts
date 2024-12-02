// run with `deno run --allow-read=input.txt main.ts`

const file = await Deno.readTextFile("input.txt");

const reports = file
  .split("\n")
  .filter((line) => line)
  .map((line) => line.split(/ +/).map((n) => parseInt(n, 10)));

const part1 = reports.filter(report => {
  let ascending = true;
  let descending = true;
  let diffOkay = true;
  for (let i = 1; i < report.length; i++) {
    const diff = report[i] - report[i - 1];
    const absoluteDiff = Math.abs(diff);
    ascending = ascending && diff >= 0;
    descending = descending && diff <= 0;
    diffOkay = diffOkay && (absoluteDiff >= 1 && absoluteDiff <= 3);
  }
  return (ascending || descending) && diffOkay;
}).length;

console.log(`Part 1: ${part1}`);
