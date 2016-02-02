-module (element_sigma_multi).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

%% Move the following line to records.hrl:
reflect() -> record_info(fields, multiselect).

	
render_element(Rec) ->
	Format = fun(R) ->
		{Value,Display} = case R of
			[Val,Dis] -> {Val,Dis};
			{Val,Dis} -> {Val,Dis}
		end,
		{Value,Display,lists:member(Value,Rec#multiselect.selected)}
	end,

	Data = lists:map(Format,Rec#multiselect.data),
	Width = case Rec#multiselect.width of
		W when is_integer(W) -> integer_to_list(W) ++ "px";
		W when is_list(W) -> W;
		_ -> ""
	end,
	MaxHeight = case Rec#multiselect.maxheight of
		H when is_integer(H) -> integer_to_list(H) ++ "px";
		H when is_list(H) -> H;
		_ -> ""
	end,
	BodyFun = case Rec#multiselect.orientation of
		horizontal -> fun horizontal_table/2;
		vertical -> fun vertical_table/2
	end,

		
	#panel{
		id=Rec#multiselect.id,
		class=[multiselect,Rec#multiselect.class],
		style="overflow:auto;width:" ++ Width ++ ";max-height:" ++ MaxHeight,
		body = BodyFun(Rec#multiselect.postbackid,Data)
	}.

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
			
horizontal_header_cell(DataRow) ->
	#tablecell{text=element(2,DataRow)}.

horizontal_data_cell(DataRow,ID) ->
	Value = element(1,DataRow),
	Selected = ?IF3(size(DataRow)==3,element(3,DataRow),false),
	#tablecell{body=[
		#checkbox{id=ID,value=wf:to_list(Value),checked=Selected}
	]}.
