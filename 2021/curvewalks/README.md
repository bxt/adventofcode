Curve Walks
===========

This project is for rendering drawings of dots wandering along Lindenmayer systems (L-Systems).

Inspiraton
----------

Recently I saw a [Tweet with an awesome animated GIF by Etienne Jacob](https://twitter.com/etiennejcb/status/1469381639134453764) containing an animation where dots wander along a [Hilbert curve](https://de.wikipedia.org/wiki/Hilbert-Kurve) in an interesting motion pattern.

Since I was visualising things with [Deno](https://deno.land/) for [Advent of Code](https://adventofcode.com/2021) anyways, I was stoked to see if I could create something similar – but with even more points. You see, in the original, there are 2^5 points in one direction, amounting to 1024 points in total, of which around half are rendered as circles. I quickly changed that to be 2^8*2^8 = 65536 points, rendering half of those as well. Here are some fun facts about my first draft:

- One point takes more than 18h to traverse the picture.
- No points ever cross paths.
- No point ever visits the same spot twice, no loops.

Since I was on Wikipedia anyways to look up the formula for the Hilbert curve, I found there are some other, similar curves, one of which fits into a approximately hexagonal space – the [Gosper curve](https://en.wikipedia.org/wiki/Gosper_curve). Now for this one, you don't find a formula that describes the position of a point, rather the algorithms are given as "L-Systems", where you generate a pattern of walking an turning.

Once my code was adjusted to work with these L-Systems, it was super easy to not only have the dots walk along a Gosper curve, but also some other ones which I found on [Paul Bourke's L-System User Notes](http://paulbourke.net/fractals/lsys/). Coincidentially, the Hilbert curve can also be described with an L-System, so I could actually throw away my carefully crafted formulas again.

Usage
-----

The code is supposed to run by calling the `bin.ts` file, the only required argument is `--curve`, e.g.:

```sh
deno run bin.ts --curve=hilbert
```

To show a list of available options, run:

```sh
deno run bin.ts --help
```

There are multiple options available for kind and complexity of the curve, size of various elements and if to draw the line along the curve as well.

Development
-----------

When developing it is probably best to keep the tests running in the background:

```sh
deno test --watch --allow-net ----allow-read
```

Other than that just make sure the following commands still work:

```sh
./bin --curve hilbert
./generateAll.sh # generates a bunch of examples in ./out
```

Spicing it up in the Browser
----------------------------

If you look at the GIF in the browser, you can paste the following into the JavaScript console to make the colors change over time:

```js
const style = document.createElement('style');
style.type = 'text/css';
style.innerHTML = `
  @keyframes hue-rotate-filter-animation {
    from {
      filter: hue-rotate(0deg);
    }

    to {
      filter: hue-rotate(360deg);
    }
  }
  body {
    animation: hue-rotate-filter-animation 400s infinite;
  }
`;
document.getElementsByTagName('head')[0].appendChild(style);
```