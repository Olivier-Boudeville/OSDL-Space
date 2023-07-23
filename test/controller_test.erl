% Copyright (C) 2023-2023 Olivier Boudeville
%
% This file is part of the OSDL-Space library.
%
% This library is free software: you can redistribute it and/or modify
% it under the terms of the GNU Lesser General Public License or
% the GNU General Public License, as they are published by the Free Software
% Foundation, either version 3 of these Licenses, or (at your option)
% any later version.
% You can also redistribute it and/or modify it under the terms of the
% Mozilla Public License, version 1.1 or later.
%
% This library is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
% GNU Lesser General Public License and the GNU General Public License
% for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License, of the GNU General Public License and of the Mozilla Public License
% along with this library.
% If not, see <http://www.gnu.org/licenses/> and
% <http://www.mozilla.org/MPL/>.
%
% Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
% Creation date: Sunday, June 18, 2023.


% @doc Test of an <b>OSDL-Space controller</b>.
-module(controller_test).


% For run/0 export and al:
-include("test_facilities.hrl").


% The sole include that MyriadGUI user code shall reference:
-include_lib("myriad/include/myriad_gui.hrl").


% Rendering section.


% Description of a simple, local, screen coordinate system:
-record( screen, {

	center :: integer_point2() } ).

-type screen() :: #screen{}.




% Shorthands:

-type message() :: basic_utils:message().


-type coordinate() :: linear:coordinate().

-type integer_point2() :: point2:integer_point2().

-type user_event_registry() :: gui_event:user_event_registry().


% Notably for the defaults:
-include("controller_defines.hrl").


% GUI section.


% State of the program, passed between event handlers.
-record( gui_state, { main_frame,
					  quit_button,
					  status_bar,
					  canvas,
					  screen :: screen(),
					  user_event_registry :: user_event_registry() } ).



% The left part of the frame shows the canvas (viewports), while the right one
% gathers the associated selectors.


-spec get_main_window_width() -> coordinate().
get_main_window_width() ->
	1920.


-spec get_main_window_height() -> coordinate().
get_main_window_height() ->
	1080.



% @doc Initialises the GUI and associated parts.
-spec start() -> no_return().
start() ->

	case gui_opengl:get_glxinfo_strings() of

		undefined ->
			test_facilities:display( "No proper OpenGL support detected on "
				"host (no GLX visual reported), controller cannot be run." ),
		% FIXME tmp:
					run_actual_test();

		GlxInfoStr ->
			case gui_opengl:is_hardware_accelerated( GlxInfoStr ) of

				true ->
					test_facilities:display( "Hardware acceleration is "
						"available, controller can be run." ),

					run_actual_test();

				false ->
					test_facilities:display( "No proper OpenGL support "
						"detected on host (no hardware acceleration reported), "
						"controller cannot be run." )

			end

	end.



run_actual_test() ->

	gui:start(),

	% May be useful:
	%observer:start(),

	% Not expected to trigger, as the first impacted will be the MyriadGUI main
	% loop:
	%
	%process_utils:spawn_message_queue_monitor( _MonitoredPid=self(),
	%   _MonitoredProcessDesc="MyriadGUI test main loop" ),

	trace_utils:notice( "Running the OSDL-Space controller test." ),

	% Starts as 1/4 of the full theoretical screen size:
	FrameSize =
		{ get_main_window_width() div 2, get_main_window_height() div 2 },

	QuitButtonId = quit_button_id,

	% We prefer here relying on keycodes rather than scancodes, as at least for
	% defaults we prefer setting the keycode 'q' for quit for example.

	% Create a table abstracting-out the various ways for the user to generate
	% events (e.g. based on remapped keys, mouse actions, etc.):
	%
	UsrEvReg = gui_event:create_user_event_registry( [
		% Trigger the following app-level event...
		{ quit_requested,
		  % ... whenever any of these user-level events happen:
		  [ { button_clicked, QuitButtonId },
				   { keycode_pressed, ?MYR_K_q },
			{ scancode_pressed, ?default_quit_scan_code },
			window_closed ] } ] ),

	MainFrame = gui:create_frame( _Title="OSDL-Space Controller",
		_FramePos=auto, FrameSize, _FrameStyle=default, _Id=main_frame_id,
		_MaybeParent=undefined ),

	gui:subscribe_to_events( { onWindowClosed, MainFrame } ),

	StatusBar = gui:create_status_bar( MainFrame ),


	gui:push_status_text( "Initialising.", StatusBar ),

	MainPanel = gui:create_panel( MainFrame ),

	SelectorPanel = gui:create_panel( MainFrame ),

	MainSizer = gui:create_sizer( horizontal ),

	% Grows with the window:
	gui:add_to_sizer( MainSizer, MainPanel,
					  [ { proportion, 2 }, expand_fully ] ),

	% Not subscribing to SelectorPanel: as this panel has child buttons, it will
	% never receive any key press.
	%
	gui:subscribe_to_events( { onKeyPressed, MainPanel } ),

	% Constant width:
	gui:add_to_sizer( MainSizer, SelectorPanel,
					  [ { proportion, 0 }, expand_fully ] ),

	ControlBoxSizer = gui:create_sizer_with_labelled_box( vertical,
		SelectorPanel, "Controls" ),

	% Adding the buttons to the control panel:

	% Common settings:

	Position = auto,
	ButtonSize = auto,
	ButtonStyle = default,

	% Parent cannot be a sizer:
	ButtonParent = SelectorPanel,

	QuitButton = gui:create_button( "Quit", Position, ButtonSize, ButtonStyle,
									QuitButtonId, ButtonParent ),

	Buttons = [ QuitButton ],

	gui:subscribe_to_events( [ { onButtonClicked, B } || B <- Buttons ] ),


	%gui:set_tooltip( MainPanel, "Controls for the selectors" ),

	[ gui:add_to_sizer( ControlBoxSizer, B, expand_fully ) || B <- Buttons ],

	gui:set_sizer( SelectorPanel, ControlBoxSizer ),

	% Would prevent the panel to receive key presses:
	%RenderBoxSizer = gui:create_sizer_with_labelled_box( vertical, MainPanel,
	%													 "World rendering" ),

	%Canvas = gui:create_canvas( MainPanel ),

	%gui:set_background_color( Canvas, red ),

	%gui:clear( Canvas ),

	%gui:subscribe_to_events( { [ onRepaintNeeded, onResized ], Canvas } ),

	%gui:add_to_sizer( RenderBoxSizer, Canvas,
	%				  [ { proportion, 1 }, expand_fully ] ),

	%gui:set_tooltip( Canvas, "Rendering of OSDL-Space." ),

	%gui:set_sizer( MainPanel, RenderBoxSizer ),

	gui:set_sizer( MainFrame, MainSizer ),

	% Focus needed to receive events:
	gui:set_focus( MainPanel ),

	% Sets the GUI to visible:
	gui:show( MainFrame ),

	% FIXME
	Screen = #screen{ center={ get_main_window_width() / 3 - 550,
							   get_main_window_height() / 2 } },


	InitialState = #gui_state{ main_frame=MainFrame,
							   quit_button=QuitButton,
							   status_bar=StatusBar,
							   %canvas=Canvas,
							   screen=Screen,
							   user_event_registry=UsrEvReg },

	% Wanting to catch up with the solvers:
	erlang:process_flag( priority, _Level=high ),

	gui:push_status_text( "Initialised.", StatusBar ),

	gui_main_loop( InitialState ).



% @doc The main loop of this test, driven by the receiving of MyriadGUI
% messages, whose events are converted to application ones.
%
gui_main_loop( GUIState=#gui_state{ user_event_registry=UsrEvReg } ) ->

	% Triggered whenever a user event is received and can be promoted to an
	% application event; blocking version:
	%
	case gui_event:get_application_event( UsrEvReg ) of

		{ quit_requested, _BaseEvent } ->
			on_quit( GUIState );

		OtherAppEvent ->
			trace_utils:warning_fmt( "Unhandled (hence ignored) application "
				"event: ~ts.",
				[ gui_event:application_event_to_string( OtherAppEvent ) ] ),
			gui_main_loop( GUIState )

	end.



% Called whenever having to quit.
on_quit( #gui_state{ main_frame=MainFrame } ) ->

	trace_utils:debug( "Quit requested." ),

	% Simply stop recursing:
	gui:destruct_window( MainFrame ),

	gui:stop().



% @doc Runs the test.
-spec run() -> no_return().
run() ->

	test_facilities:start( ?MODULE ),

	case executable_utils:is_batch() of

		true ->
			test_facilities:display( "(not running the OSDL-Space "
				"controller, being in batch mode)" );

		false ->
			start()

	end,
	test_facilities:stop().
