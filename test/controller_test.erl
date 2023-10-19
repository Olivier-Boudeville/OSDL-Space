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


% Notably for the defaults:
-include("controller_defines.hrl").



% GUI section.


% Application-specific GUI information, to be stored in the app_specific_info
% field of the MyriadGUI app_gui_state record, and to be passed between event
% drivers.
%
-record( controller_gui_info, {

	% The main frame of this test:
	main_frame :: frame(),

	% To quit thanks to a button:
	quit_button :: button(),

	% Bottom status bar:
	status_bar :: status_bar(),

	% Information regarding the displayed screen:
	screen :: screen() } ).

-type controller_gui_info() :: #controller_gui_info{}.
% Application-specific GUI information, to be stored in the app_specific_info
% field of the app_gui_state record, and to be passed between event drivers.


% Silencing:
-export_type([ controller_gui_info/0 ]).



% Shorthands:

-type message() :: basic_utils:message().

-type width() :: gui:width().
-type height() :: gui:height().

-type integer_point2() :: point2:integer_point2().

-type frame() :: gui_frame:frame().
-type button() :: gui_button:button().
-type status_bar() :: gui_statusbar:status_bar().

-type gl_canvas() :: gui_opengl:gl_canvas().
-type gl_context() :: gui_opengl:gl_context().

-type user_event_registry() :: gui_event:user_event_registry().



% The left part of the frame shows the canvas (viewports), while the right one
% gathers the associated selectors.


-spec get_main_frame_width() -> width().
get_main_frame_width() ->
	1920.


-spec get_main_frame_height() -> height().
get_main_frame_height() ->
	1080.



% @doc Initialises the GUI and associated parts.
-spec start() -> no_return().
start() ->

	case gui_opengl:get_glxinfo_strings() of

		undefined ->
			test_facilities:display( "No proper OpenGL support detected on "
				"host (no GLX visual reported), controller cannot be run." );

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

	test_facilities:display( "This test will display a frame "
		"comprising a main 3D view, and controls on the right." ),

	% May be useful:
	%observer:start(),

	% Not expected to trigger, as the first impacted will be the MyriadGUI main
	% loop:
	%
	%process_utils:spawn_message_queue_monitor( _MonitoredPid=self(),
	%   _MonitoredProcessDesc="MyriadGUI test main loop" ),

	trace_utils:notice( "Running the OSDL-Space controller test." ),

	gui:start(),

	% Could be batched (see gui:batch/1) to be more effective.
	%InitialGUIState = gui:batch( fun() -> init_test_gui() end ),

	% Starts as 1/4 of the full theoretical screen size:
	FrameSize =
		{ get_main_frame_width() div 2, get_main_frame_height() div 2 },

	QuitButtonId = quit_button_id,

	% We prefer here relying on keycodes rather than scancodes, as at least for
	% defaults we prefer setting the keycode 'q' for quit for example.

	% Create a table abstracting-out the various ways for the user to generate
	% events (e.g. based on remapped keys, mouse actions, etc.):
	%
	InitAppGUIState = gui_event:create_app_gui_state( [
		% Trigger the following app-level event...
		{ quit_requested,
		  % ... whenever any of these user-level events happen:
		  [ { button_clicked, QuitButtonId },
			{ keycode_pressed, ?MYR_K_q },
			{ scancode_pressed, ?default_quit_scan_code },
			window_closed ] } ], _UseOpenGL=true ),

	% Overrides this default with our driver:
	ShowAppGUIState = gui_event:set_event_driver( onShown, onShown_driver/2,
												  InitAppGUIState ),

	MainFrame = gui_frame:create( _Title="OSDL-Space Controller",
		_FramePos=auto, FrameSize, _FrameStyles=[ default ], _Id=main_frame_id,
		_MaybeParent=undefined ),

	% This test may request additionally an OpenGL debug context:
	%GLAttrs = gui_opengl:get_default_canvas_attributes(),
	GLAttrs = [ debug_context | gui_opengl:get_default_canvas_attributes() ],

	GLCanvas = gui_opengl:create_canvas( _Parent=MainFrame,
		_CanvasAttrs=[ { gl_attributes, GLAttrs } ] ),

	% Created, yet not bound yet (must wait for the main frame to be shown) (so
	% GL context cannot be set as current yet):
	%
	GLContext = gui_opengl:create_context( GLCanvas ),

	GLAppGUIState = ShowAppGUIState#app_gui_state{
		opengl_state={ GLCanvas, GLContext } },


	gui:subscribe_to_events( { [ onResized, onShown, onWindowClosed ],
							   MainFrame } ),

	% Needed, otherwise if that frame is moved out of the screen or if another
	% windows overlaps, the OpenGL canvas gets garbled and thus must be redrawn:
	% (onResized a priori not needed)
	%
	gui:subscribe_to_events( { onRepaintNeeded, GLCanvas } ),

	StatusBar = gui_statusbar:create( MainFrame ),

	gui_statusbar:push_text( StatusBar, "Initialising." ),

	MainPanel = gui_panel:create( MainFrame ),

	SelectorPanel = gui_panel:create( MainFrame ),

	MainSizer = gui_sizer:create( _Orient=horizontal ),

	% Grows with the window:
	gui_sizer:add_element( MainSizer, MainPanel,
						   [ { proportion, 2 }, expand_fully ] ),

	% Not subscribing to SelectorPanel: as this panel has child buttons, it will
	% never receive any key press.
	%
	gui:subscribe_to_events( { onKeyPressed, MainPanel } ),

	% Constant width:
	gui_sizer:add_element( MainSizer, SelectorPanel,
						   [ { proportion, 0 }, expand_fully ] ),

	ControlBoxSizer = gui_sizer:create_with_labelled_box( vertical, "Controls",
														  SelectorPanel ),

	% Adding the buttons to the control panel:

	% Common settings:

	Position = auto,
	ButtonSize = auto,
	ButtonStyles = [],

	% Parent cannot be a sizer:
	ButtonParent = SelectorPanel,

	QuitButton = gui_button:create( "Quit", Position, ButtonSize, ButtonStyles,
									QuitButtonId, ButtonParent ),

	Buttons = [ QuitButton ],

	gui:subscribe_to_events( { onButtonClicked, Buttons } ),


	%gui_widget:set_tooltip( MainPanel, "Controls for the selectors" ),

	gui_sizer:add_elements( ControlBoxSizer, Buttons, expand_fully ),

	gui_widget:set_sizer( SelectorPanel, ControlBoxSizer ),

	% Would prevent the panel to receive key presses:
	%RenderBoxSizer = gui_sizer:create_with_labelled_box( vertical, 
    %    "World rendering", MainPanel ),

	% Same:
	%RenderBoxSizer = gui_sizer:create_with_box( vertical, MainPanel ),

	% Only one preserving key presses:
	%RenderBoxSizer = gui_sizer:create( vertical ),

	%gui:subscribe_to_events( { onKeyPressed, Canvas } ),

	%gui_widget:set_background_color( Canvas, red ),

	%gui_canvas:clear( Canvas ),

	gui_sizer:add_element( RenderBoxSizer, GLCanvas,
						   [ { proportion, 1 }, expand_fully ] ),

	%gui_widget:set_tooltip( Canvas, "Rendering of OSDL-Space." ),

	gui_widget:set_sizer( MainPanel, RenderBoxSizer ),

	gui_widget:set_sizer( MainFrame, MainSizer ),

	% Focus needed to receive events; both components work:
	gui_widget:set_focus( MainPanel ),
	%gui_widget:set_focus( GLCanvas ),

	% Sets the GUI to visible:
	gui_frame:show( MainFrame ),

	% FIXME
	Screen = #screen{ center={ get_main_frame_width() / 3 - 550,
							   get_main_frame_height() / 2 } },

	InitGUIInfo = #controller_gui_info{ main_frame=MainFrame,
										quit_button=QuitButton,
										status_bar=StatusBar,
										screen=Screen },

	InfoAppGUIState =
		GLAppGUIState#app_gui_state{ app_specific_info=InitGUIInfo },


	% Wanting to catch up with the solvers:
	erlang:process_flag( priority, _Level=high ),

	gui_statusbar:push_text( "Initialised.", StatusBar ),

	% OpenGL will be initialised only when the corresponding frame will be ready
	% (that is once first reported as resized):
	%
	gui_main_loop( InfoAppGUIState ).




% @doc The main loop of this test, driven by the receiving of MyriadGUI
% messages, whose events are converted to application ones.
%
% OpenGL will be initialised only when the corresponding frame will be ready
% (that is once first reported as resized).
%
-spec gui_main_loop( app_gui_state() ) -> no_return().
gui_main_loop( AppGUIState ) ->

	% Triggered whenever a user event is received and can be promoted to an
	% application event; blocking version:
	%
	case gui_event:get_application_event( AppGUIState ) of

		{ { quit_requested, _BaseEvent }, QuitAppGUIState } ->
			on_quit( QuitAppGUIState );

		{ { OtherAppEvent, _BaseEvent }, OtherAppGUIState } ->
			trace_utils:warning_fmt( "Unhandled (hence ignored) application "
				"event by this controller:~n~ts.",
				[ gui_event:application_event_to_string( OtherAppEvent ) ] ),
			gui_main_loop( OtherAppGUIState );

		% Extra safety:
		OtherAppEventReturn ->
			trace_utils:error_fmt( "Unexpected (hence ignored) application "
				"event return received by this controller (abnormal):~n~p.",
				[ OtherAppEventReturn ] ),
			throw( { invalid_app_event_return, OtherAppEventReturn } )

	end.



% @doc Application-specified driver overriding default_onShown_driver/2.
%
% The most suitable first location to initialise OpenGL, as making a GL context
% current requires a shown window.
%
onShown_driver( _Elements=[ Frame, FrameId, EventContext ],
				AppGUIState=#app_gui_state{ use_opengl=false } ) ->

	trace_utils:debug_fmt( "Controller driver: parent window (main frame) "
		"just shown (initial size of ~w).", [ gui_widget:get_size( Frame ) ] ),

	% Done once for all:
	InitAppGUIState = initialise_opengl( AppGUIState ),

	% Optional yet better:
	gui:unsubscribe_from_events( { onShown, Frame } ),
	fixme.




% @doc Sets up OpenGL, once for all, once a proper OpenGL context is available.
-spec initialise_opengl( app_gui_state() ) -> app_gui_state().
initialise_opengl( GUIState=#my_gui_state{ canvas=GLCanvas,
										   context=GLContext,
										   % Check:
										   opengl_initialised=false } ) ->

	% Initial size of canvas is typically 20x20 pixels:
	trace_utils:debug_fmt( "Initialising OpenGL (whereas canvas is of initial "
						   "size ~w).", [ gui_widget:get_size( GLCanvas ) ] ),

	% So done only once:
	gui_opengl:set_context_on_shown( GLCanvas, GLContext ),

	test_opengl_debug_context(),

	%Exts = gui_opengl:get_supported_extensions(),
	%trace_utils:info_fmt( "~B OpenGL extensions supported: ~ts",
	%   [ length( Exts ), text_utils:atoms_to_listed_string( Exts ) ] ),

	% These settings will not change afterwards here (set once for all):

	% Clears in black:
	gl:clearColor( 0.0, 0.0, 0.0, 0.0 ),

	% Draws in white:
	gl:color3f( 1.0, 1.0, 1.0 ),

	gl:matrixMode( ?GL_PROJECTION ),
	gl:loadIdentity(),

	% Multiplies the current modelview matrix by an orthographic matrix, a
	% perspective matrix that produces a parallel projection based on 6 clipping
	% planes, implementing the MyriadGUI 2D conventions.
	%
	% Here coordinates are normalised in [0.0,1.0] and as such are
	% definition-independent (resizing the frame and then the viewport will not
	% affect them).
	%
	% Like glu:ortho2D/4:
	%
	gl:ortho( _Left=0.0, _Right=1.0, _Bottom=1.0, _Top=0.0, _Near=-1.0,
			  _Far=1.0 ),

	%trace_utils:debug_fmt( "Managing a resize of the main frame to ~w.",
	%                       [ gui:get_size( MainFrame ) ] ),

	InitGUIState = GUIState#my_gui_state{ opengl_initialised=true },

	% As the initial onResized was triggered whereas no OpenGL state was
	% already available:
	%
	on_main_frame_resized( InitGUIState ).



% Called whenever having to quit.
on_quit( #gui_state{ main_frame=MainFrame } ) ->

	trace_utils:debug( "Quit requested." ),

	% Simply stop recursing:
	gui_frame:destruct( MainFrame ),

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
