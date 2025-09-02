We're working on prototyping a rewrite of `domaindriven` to use `Effectful`. 

## The idea

domaindriven is currently implementing commands using custom servant combinators, `CmdI`, `CbCmdI`, `QueryI`, etc. I want to stop doing that and just use the normal servant endpoint combinators, `Post`, `Query`. This seems super obvious right now, but the history of the library put us in this spot. 

The current prototype use 3 parameters to the Effects. I don't like it, but I'm not sure how to do it without the parameters. I guess it can be simplified using funtional dependencies, `model -> event, model -> index`.



## Work so far

The initial effects are in:

- domaindriven-effectful/src/DomainDriven/Effectful/Aggregate.hs
- domaindriven-effectful/src/DomainDriven/Effectful/Projection.hs


