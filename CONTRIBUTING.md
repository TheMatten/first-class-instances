# Building

To build with `stack`, copy `stack-XYZ.yaml` file to `stack.yaml`, based on
version of GHC you want to work with.

# Code style

There are no concrete guidelines to follow, but try to keep style consistent
with existing code. We may sometimes suggest changes in formatting in merge
requests.

# Testing

If your change introduces new features, it may be good idea to add new tests
that make sure it works as it should - project uses CI that checks your MRs
automatically with supported versions of GHC.
