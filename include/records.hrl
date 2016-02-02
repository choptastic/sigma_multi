-record(sigma_multi, {?ELEMENT_BASE(element_sigma_multi),
    wrapperid               :: id(),
    width=""                :: integer() | list(),
    maxheight=""            :: integer() | list(),
    data=[]                 :: [{text(), text()}],
    selected=[]             :: [text() | integer()],
    orientation=vertical    :: horizontal | vertical
}).
