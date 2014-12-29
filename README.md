# parabolic-pde

A solver for the heat equation in a neat package to generate pretty gifs with gnuplot.

### Description

I haven't read all the mathematics about PDEs yet, but I do have some sort of
intuition about them. And I sure as hell want to make some pretty gifs with them.
[Like this one.](https://upload.wikimedia.org/wikipedia/commons/a/a9/Heat_eqn.gif)

What's the meaning of them? Well it's a 2d plate with temperature of it plotted
as a third third dimmension. More on that
[in wikipedia](https://en.wikipedia.org/wiki/Heat_equation).

## Requirements

ghc, cabal, gnuplot, imagemagick, linux(well, something unixy, really.) Also, **run** these:

    # cabal update # might be needed, if dependency tree exhausted.
    cabal install --dependencies-only
    ghc -O2 --make -threaded -rtsopts parabolic-pde
    (cd plot; ghc datify.hs)

### Usage

I use directories input and output for storing data, so they're in .gitignore.
I've also added some sample data in samples directory.

To **make** the animation, you'll need to generate snapshots of the computation.
Which took about a minute and a ½ on my two core, 2.3 GHz intel processor. (1.)
And plot, and plot them. (2.) That might look something like this:

    ./parabolic-pde 1000 +RTS -N2 < samples/moon* # 1.
    mkdir output
    mv frame* output
    ./plot/main output 50 50 # 2.
    ls plot/animation.gif

If you'll try that again, you'll need to empty out plot/plots/\*, but that's pretty much it.

### Issues

Some of the executable files are not fool-proof, so if you run them without
the right arguments, they might spit a <s>hairy ball of goo</s> senseless
errors. The executables parabolic-pde and plot/main should work right, though.

## Pretty pictures

From

![input](https://raw.github.com/siers/parabolic-pde/master/samples/readme-moon.png)

it generated

![output](https://raw.github.com/siers/parabolic-pde/master/samples/animation-fastest.gif)

## License

If you need it, steal it. It's not much code anyway.
