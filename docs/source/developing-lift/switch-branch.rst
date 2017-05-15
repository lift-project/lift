Switching branch
================

To switch branches, you can either use IntelliJ or the command line using the following command::

    > git checkout BRANCHNAME

If the branch does not appear in IntelliJ, you may have to first ``fetch`` the remote information about the repository.
This can be done directly in IntelliJ or via the command line::

    > git fetch

When switching from one branch to another, it is important to ensure that the ``ArithExpr`` submodule is being updated.
Since IntelliJ does not support well submodules, you should type on the command line::

    > git submodule update

after having switched branches.
