# Aperiodic

This is a Haskell implementation of the algorithms proposed in the [four part series](https://www.chiark.greenend.org.uk/~sgtatham/quasiblog/aperiodic-tilings/) blog post by Simon Tatham. You can also read my own presentation [here](https://trebor-huang.github.io/forest/tile-0001.xml).

This depends on the [Kleenex](https://github.com/diku-kmc/kleenexlang/) program. I am using a custom local fork for now, so you need to edit the extra dependencies in `stack.yaml` yourself. You may also need to edit the dependency information of that library to make it compile.

## Algorithms

We have three algorithms `Signature tile subtile -> edge -> Maybe (Signature tile subtile, edge)` that generate the combinatorial structure of the tilings.

- The recursive algorithm is the simplest, but it hangs on some infinite inputs. On finite inputs, it sometimes only produces partial results, i.e. some tiles are determined by the input, but the algorithm cannot recognize them and gives up.
- The naive transductive algorithm is very slow, needs to read the entire input before producing the answer (and hence does not handle any infinite input), but it will calculate all the tiles that are determined by the input (and connected to the starting tile).
- The streaming transductive algorithm combines the pros of the previous two. It is about as fast than the recursive algorithm, handles infinite inputs and produces the complete answer.

Some periodic infinite inputs can't be handled because even the first token of the output cannot be determined without knowing about the periodicity. Since the input format is an arbitrary infinite list, no algorithm can handle this. I might write another algorithm that specifically deals with these rational inputs, by including the period information in the input format.
