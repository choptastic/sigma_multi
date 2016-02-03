-record(sigma_multi, {?ELEMENT_BASE(element_sigma_multi),
    wrapperid               :: id(),
    width=""                :: integer() | list(),
    maxheight=""            :: integer() | list(),
    options=[]              :: [{text(), text()} | #option{}],
    selected=[]             :: [text() | integer()],
    orientation=vertical    :: horizontal | vertical
}).
