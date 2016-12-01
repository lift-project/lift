# How to Contribute

Although Lift is a primarily research focused project, treating it as the complex software project it is, and therefore maintaining a number of software development practices is in all our interests. With these practices, we aim to interrupt fast iteration of design and code as little as possible, while at the same time maintain sufficient stability (of APIs etc) to allow other users to develop features independently. 

This document describes our core guidelines for working with this project, how new features and changes should be developed, and added to the core codebase.

<!-- For future reference, the first section of this document describes some of the previous practice that prompted our adoption of the contributing polices. The policies are informal, and not set in stone, hence the justification. -->

## Development workflow

This project operates along using fairly standard branch, pull request, and merge git methodology. In essence, all changes to the project should be made in branches of the project, which will be merged back into the master branch after acceptance via pull request. This process should be lightweight enough that creating new features requires as little effort as possible, but restrictive enough that large breaking changes are rare, and occur predictably enough that other users can comment on, and adapt to them. 

### Basic features

The basic workflow for adding a single small feature (e.g. a standalone benchmark, without any compiler modifications) to the lift project is as follows: 

1. Create a feature branch from the master branch, i.e. `git checkout master && git checkout -b feature_branch`

2. Make changes within the feature branch. 

3. Make sure all tests pass locally, and on Jenkins

4. Open a pull request on bitbucket, from the feature branch (`feature_branch`) to master

5. Wait for comments, and if positive, merge it in. 

### Larger projects

Unfortunately, it is rare that we have the luxury of producing one small standalone feature/piece of work, which can be easily described in a single branch, and developed separately. More often than not, we have a separate larger project (e.g. a paper that we're writing) that has multiple separate interacting components that we wish to develop. In this case, although we might want to develop on a single branch, and push all the changes once we're done, that can be frustrating for other users, as the branch may contain a large number of breaking changes that are difficult or time consuming to merge and fix.

We therefore advise that *where possible*, all changes should be broken out into separate feature branches as soon as possible, and pull requests to master submitted. For example, say I have a branch `big_paper`, which is up to date with master (i.e. is no commits behind master, and several ahead), and a feature that could be separately added to the master branch. In this instance, we would prefer that the changes that make up such a feature are added to a separate branch from `big_paper`, and a pull request submitted to master. 

The alternative, submitting a large pull request at the end of a project, is unfavorable, as it produces a lot of work for other developers, and often involves lots of complex changes. In these instances, we would request that at the end of a large project, a developer splits any new additions into separate feature branches, which then all have independent pull requests opened. This creates more work on the behalf of the feature developer, yes, but allows maintainers to more selectively and carefully accept or delay features that might cause issues to the project.  

## Pull requests

All changes and additions to the Lift project must be done in the form of a pull request from a branch or fork. In addition, we are fairly opinionated with regards to what makes a "good" pull request, but also flexible enough to recognise that as this is a research project, a "good" pull request won't always be possible to produce. 

As a general guideline, we don't want pull requests that are "too big". What this means, exactly, is fairly hazy, but in general a pull request should:

1) Add a single feature, or change, to the project (features include the associated tests and minor changes that they might require). This could be a change in how we generate code for a parallel pattern, the addition of a new pattern, a benchmark (without any new changes to the compiler - they should have been separate pull requests previously), or a new pattern. Ideally, 

2) Be small enough, or contain little enough code, that a reviewer can read and understand it within an hour. This is an arbitrary deadline, that shouldn't be taken seriously, but which sets the tone for the amount of code that is "too much".

Of course, in cases where a number of features, or changes, are highly coupled, then it is inevitable that they will all be submitted together. In this case, simply be prepared for the pull request to take longer to be approved.

### Breaking changes

In some cases, a pull request will contain code that fundamentally changes API, or some other structure of the code that may potentially impact the work of other developers. In this case, our aim is to minimise this impact, by notifying other developers, and delaying the merge until we are sure that it is necessary.  As a developer you can also help by reducing the size of the pull request as much as possible to mitigate any potential breakages. Of course, this is not always possible. 