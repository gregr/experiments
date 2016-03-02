# Example-based programming that mixes symbolic and direct manipulation


## Case study: programming an interactive goban


### Draw static content, then generalize

* board
    * draw background
    * draw grid lines
    * listify line rows and columns
    * parameterize and constrain list sizes (num rows must equal num cols)
    * parameterize spacing dimensions
    * draw star points
    * describe and parameterize star point placement conditions in terms of board size
* stones
    * draw colorless base stone
    * parameterize dimensions
    * instantiate and draw black and white versions
* board + stones
    * instantiate and snap stones of both colors over board intersections
    * listify these stones/positions
    * transform list -> dict (pos => stone)
        * also indicates position uniqueness
    * parameterize over the stone dict
    * factor stone absolute position dimensions in terms of intersection position
    * parameterize over dimension factor


### Demonstrate dynamic rules, then generalize

* stone placement
    * correlate mouse click with appropriate intersection position
    * if position is not empty, do nothing
    * otherwise, correlate clicked intersection with stone placement
* removal
    * correlate a placed stone with starting a new process
    * remember stone
    * identify and remember stone neighbors
        * if any neighbor is empty, full process ends successfully
        * for each non-empty neighbor of the same color, recurse
    * if process does not end successfully before we run out of candidate neighbors to follow, process ends in failure
        * all remembered stones (those traversed of the same color) are removed
* capture
    * correlate stone placement intersection with neighboring intersections
    * identify neighbors of opposite color
    * with each opposer, attempt removal process
    * finally, attempt removal process on newly placed stone
* playing a move
    * start a list with empty goban
    * add new goban formed by placing a stone
    * generalize as procedure taking history list, stone, and placement position, producing a new history list
* alternating play
    * correlate stone placement with color alternation
* comparing two gobans
    * compare each corresponding intersection to determine whether all are identical
* positional super ko
    * extend play-move procedure by comparing the newly-created goban with all gobans already in the history
    * if any are identical to the new goban, fail
    * otherwise, succeed with the new history
