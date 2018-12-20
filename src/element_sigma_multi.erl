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
                    selected=Selected, options=Options0, width=Width0,
                    maxheight=MaxHeight0, orientation=Orientation}) ->
	Options = normalize_and_format(Selected, Options0),
	Width = normalize_dimension(Width0),
    MaxHeight = normalize_dimension(MaxHeight0),
	#panel{
		id=wrapper_id(ID),
		class=[multiselect,Class],
		style=[Style, "overflow:auto;width:",Width,";max-height:",MaxHeight,";"], 
		body = draw_body(ID, Options, Orientation)
	}.

wrapper_id(ID) ->
    <<"sigma_multi_",(wf:to_binary(ID))/binary>>.

normalize_dimension(X) when is_integer(X) -> wf:to_list(X) ++ "px";
normalize_dimension(X) when is_list(X); is_binary(X) -> X;
normalize_dimension(_) -> "".

normalize_and_format(Selected, Options) ->
    lists:map(fun
        ({Val, Label}) ->
            {Val, Label, lists:member(Val, Selected)};
        (Opt = #option{}) ->
            {Opt#option.value, Opt#option.text, Opt#option.selected};
        (Group = #option_group{}) ->
            Group#option_group{options=
                normalize_and_format(Selected, Group#option_group.options)
            }
    end, Options).

draw_body(ID, Options, vertical) ->
    vertical_table(ID, Options);
draw_body(ID, Options, horizontal) ->
    horizontal_table(ID, Options).

vertical_table(ID, Options) ->
	#table{rows=vertical_rows(ID, Options)}.

vertical_rows(ID, Options) ->
		[vertical_row(X,ID) || X <- Options].

vertical_row(#option_group{text=Text, options=Opts}, ID) ->
    Header = #tablerow{cells=[
        #tableheader{colspan=2, style="font-size:1.4em", text=Text}
    ]},
    [Header | vertical_rows(ID, Opts)];
vertical_row({Value,Display},ID) ->
	vertical_row({Value,Display,false},ID);
vertical_row({Value,Display,Checked},ID) ->
    CheckClass = wf:temp_id(),
    LabelClass = wf:temp_id(),
    MaybeBold = ?WF_IF(Checked, "font-weight:bold"),
	#tablerow{cells=[
		#tablecell{body=[
			#checkbox{
                id=ID,
                value=wf:to_list(Value),
                checked=Checked,
                class=CheckClass,
                actions=#event{type=click, actions=[
                    "if(objs('me').prop('checked')) {
                        $('." ++ LabelClass ++ "').css('font-weight','bold');
                    }else{
                        $('." ++ LabelClass ++ "').css('font-weight','normal');
                    }"
                ]}
            }
		]},
		#tablecell{body=[
            #panel{text=Display, class=LabelClass, style=["cursor:pointer;", MaybeBold], actions=[
                #event{type=click, actions="$('." ++ CheckClass ++ "').click();"}
            ]}
        ]}
	]}.

horizontal_table(ID,Options) ->
	#table{rows=[
		#tablerow{cells=[horizontal_header_cell(Option) || Option <- Options]},
		#tablerow{cells=[horizontal_data_cell(Option,ID) || Option <- Options]}
	]}.
			

%% TODO: PROPERLY HANDLE OPTION GROUPS WITH HORIZONTAL LAYOUT
horizontal_header_cell({_,Label,_}) ->
	#tablecell{text=Label}.

horizontal_data_cell({Value,_,Selected},ID) ->
	#tablecell{body=[
		#checkbox{id=ID,value=wf:to_list(Value),checked=Selected}
	]}.
