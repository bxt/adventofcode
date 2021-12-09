<?php

// Run with e.g.:
// docker run -it --rm -v "$PWD":/usr/src/myapp -w /usr/src/myapp php:8.1.0-cli php main.php

ini_set('memory_limit', '256M');

$input = 3001330;
// $input = 5;

$next_elves = [];

$next_elves[$input] = 1;
for ($i = 1; $i < $input; $i++) {
  $next_elves[$i] = $i + 1;
}

$elves_with_gifts = $input;

$current_elf = 1;

while ($elves_with_gifts > 1) {
  $steal_from = $next_elves[$current_elf];
  $next_elf = $next_elves[$steal_from];
  $next_elves[$current_elf] = $next_elf;
  // echo "Elf $current_elf steals everything from $steal_from!\n";
  $current_elf = $next_elf;
  $elves_with_gifts--;
}

$result = $current_elf;

echo "Part 1: $result\n";



$next_elves = [];

$next_elves[$input] = 1;
for ($i = 1; $i < $input; $i++) {
  $next_elves[$i] = $i + 1;
}

$elves_with_gifts = $input;

$current_elf = 1;

while ($elves_with_gifts > 1) {
  $one_before_steal_from = $current_elf;
  $steps_to_take = floor($elves_with_gifts / 2.0);
  for ($i = 1; $i < $steps_to_take; $i++) {
    $one_before_steal_from = $next_elves[$one_before_steal_from];
  }
  $steal_from = $next_elves[$one_before_steal_from];
  $next_elves[$one_before_steal_from] = $next_elves[$steal_from];
  // echo "Elf $current_elf steals everything from $steal_from! ($steps_to_take / $elves_with_gifts)\n";
  $current_elf = $next_elves[$current_elf];
  $elves_with_gifts--;

  if($elves_with_gifts % 1000 == 0) {
    echo "$elves_with_gifts elves with gifts...\n";
  }
}

$result = $current_elf;

echo "Part 2: $result\n";



