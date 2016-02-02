%% vim: ft=nitrogen
-module (element_sigma_multi).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

-export([
    reflect/0,
    transform_element/1,
    wrapper_id/1
]).

%% Move the following line to records.hrl:
reflect() -> record_info(fields, sigma_multi).

transform_element(_Rec = #sigma_multi{id=ID, class=Class, style=Style,
                    selected=Selected, data=Data0, width=Width0,
                    maxheight=MaxHeight0, orientation=Orientation}) ->
	Data = normalize_and_format(Selected, Data0),
	Width = normalize_dimension(Width0),
    MaxHeight = normalize_dimension(MaxHeight0),
	#panel{
		id=wrapper_id(ID),
		class=[multiselect,Class],
		style=[Style, "overflow:auto;width:",Width,";max-height:",MaxHeight,";"], 
		body = draw_body(ID, Data, Orientation)
	}.

wrapper_id(ID) ->
    <<"sigma_multi_",(wf:to_binary(ID))/binary>>.

normalize_dimension(X) when is_integer(X) -> wf:to_list(X) ++ "px";
normalize_dimension(X) when is_list(X); is_binary(X) -> X;
normalize_dimension(_) -> "".

normalize_and_format(Selected, Data) ->
    lists:map(fun({Val, Label}) ->
        {Val, Label, lists:member(Val, Selected)}
    end, Data).

draw_body(ID, Data, vertical) ->
    vertical_table(ID, Data);
draw_body(ID, Data, horizontal) ->
    horizontal_table(ID, Data).

vertical_table(ID,Data) ->
	#table{rows=
		[vertical_row(X,ID) || X<-Data]
	}.

vertical_row({Value,Display},ID) ->
	vertical_row({Value,Display,false},ID);
vertical_row({Value,Display,Checked},ID) ->
	#tablerow{cells=[
		#tablecell{body=[
			#checkbox{id=ID,value=wf:to_list(Value),checked=Checked}
		]},
		#tablecell{body=Display}	
	]}.

horizontal_table(ID,Data) ->
	#table{rows=[
		#tablerow{cells=[horizontal_header_cell(DataRow) || DataRow <- Data]},
		#tablerow{cells=[horizontal_data_cell(DataRow,ID) || DataRow <- Data]}
	]}.
			
horizontal_header_cell({_,Label,_}) ->
	#tablecell{text=Label}.

horizontal_data_cell({Value,_,Selected},ID) ->
	#tablecell{body=[
		#checkbox{id=ID,value=wf:to_list(Value),checked=Selected}
	]}.
