# random-sleep
Sleep for a (configurable) random amount of time.

# Rationale
Solitary, self-isolated life under 👾COVID-19 is too deterministic. Spice up your life with **REAL** randomness 🤯! For instance, to take a coffee break in *about* half-an-hour, I use something like:
```
rlseep 30m 5m && play sound.ogg
```
See below for the usage in more detail.

# Usage
```
rsleep [MU] [SIGMA]
```
Pause for a random number of seconds.

The random number of seconds is sampled from a normal distribution of mean `MU` and standard deviation `SIGMA`.
`MU` and `SIGMA` are arbitrary floating point numbers, with an optional suffix that may be `s` for seconds (the default),
`m` for minutes, `h` for hours or `d` for days.

The random data is currently obtained from the [ANU Quantum Random Numbers Server](http://qrng.anu.edu.au).
