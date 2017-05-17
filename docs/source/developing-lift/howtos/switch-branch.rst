… switch branch
---------------

To switch/create/delete branches, you can either use IntelliJ or the command
line using the following command:

- Create a new branch at the current commit (and switch to it)::

    > git checkout -b BRANCHNAME

- Create a new branch configured to follow a remote branch::

    > git checkout -b BRANCHNAME origin/REMOTE_BRANCHNAME

- Switch to an existing branch::
  
    > git checkout BRANCHNAME
  
  **NB**: If ``BRANCHNAME`` does not exist locally, git will try the following::

    git checkout -b BRANCHNAME origin/BRANCHNAME

- Delete a branch::
  
    git branch -d BRANCHNAME

**NB**: you can only create and delete local branches this way.

Try to always stay up-to-date with bitbucket using the ``fetch`` command before
any switching to get the remote information about the repository.  This can be
done directly in IntelliJ or via the command line::

    > git fetch

When switching from one branch to another, it is important to ensure that the ``ArithExpr`` submodule is being updated.
Since IntelliJ does not support well submodules, you should type on the command line::

    > git submodule update

After having switched branches. Note that::

    > ./updateSubmodules.sh

Will do the same but will show more detailed information.

