-record(sigma_multi, {?ELEMENT_BASE(element_sigma_multi),
    wrapperid               :: id(),
    width=""                :: integer() | list(),
    maxheight=""            :: integer() | list(),
    data=[]                 :: [{value(), label()}],
    selected=[]             :: [value()],
    orientation=vertical    :: horizontal | vertical
}).
