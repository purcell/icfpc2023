# Solo entry for [ICFP Contest 2023](https://icfpcontest2023.github.io/)

I entered the contest again this year, with team name "sanityinc". For
a change I used [OCaml](), which I used a little two years ago, and wanted
to revisit.

Check out [bin/main.ml](./bin/main.ml) for the full 130-ish lines of
code. This was enough to be in the 80th percentile of teams by the end
of the lightning round (24 hours), continuing around that point late into the second
day — at the end of which I dropped out — and then dropping to around
65th percentile as other teams continued in the final 20 hours or so.

The strategy I used was a simple one: create a staggered (offset) grid
of potential placements for musicians on the stage. Precompute all
potential scores for a single instrument of each type at each position
on the grid. Then, work through all the highest-scoring
position/instrument pairings and assign musicians to them until all
the musicians are placed.

This was very fast to do, and all 90 problems could be computed in
around 10 minutes on a MacBook Air.

One basic variation I tried was different grid densities: making the
grid slightly denser or looser helped for some problems, but hindered
for others.

Once volumes were added, I (of course) assigned maximum volume to all
musicians, but zero volume to those whose placements had negative
scores. (As an early implementer of this rule I was briefly 3rd on the
leaderboard, haha!)

In a branch I tried considering the pillars when precomputing scores
for instrument/position pairs, but it didn't seem to help, and there's
a good chance I messed something up.

As a solo entrant you can't do everything, but even so, I should have
written a visualiser early. While I was surprised how good the overall
result was, I didn't arrive at any intuitions about how to solve the
problem in a more sophisticated way, and this would have been helped
by having a picture of what the problems looked like and how the
solver was tackling them. (This is a note I repeatedly write to
myself.)

At no point did I write an accurate scoring function for the entire
state that would take into account blocks, pillars and proximity. This
made it harder to even begin optimising placements.

If I'd had a few more days and more energy, I'd have written such a
function and then tried a more stochastic approach to optimise the
results of the initial placements.
