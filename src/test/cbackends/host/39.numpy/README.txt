

* unwrap: can not find its math definition,
          not done yet.

* nanprod: should map nan at python level to 1,
           before calling lift code.

* nansum: should map nan at python level to 0,
           before calling lift code.

* ediff1d: tested only on 1D, to handle parameters 
             for to_begin and to_end, array concatenation
	     should be performed at python level
	     after calling Lift code.
            

* gradient: the first element and the last element should be set manually at python level after lift code returns
    - out[0] = in[0]
    - out[last] = in[last] - in[last - 1

* i0: can nto find its math definition,
      not done yet.

* spaceing: can not find its math definition,
            not done yet.

* multiply: lift handles 1D array, please handle
            other dimensionalities at python level,
	    e.g., for each one-dimension array,
	    call lift code

* convolve: assume M is already reversed,
            please reverse the array at python level
	    before pass the parameters to lift numpy functions.

* clip: please use numpy impl, as do not know if Lift
        has a filter pattern.
	- they do, will do later if I have time

* helviside: can not find its math definition,
             thus not done jet, please use numpy impl.

* nan_to_num: please use numpy impl, as C++ don't have a NaN respresentation

* real_if_close: please use numpy impl, lift can not handle
                 the output arrays whose size is run-time dependent

* interp: the values to be interpolated must be within the bounds
          of the x coordinates, if one or more of the values is
	  out of bounds, please use the default numpy impl.
