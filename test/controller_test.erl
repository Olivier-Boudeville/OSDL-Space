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
%
% It relies on the applicative mode of operation of MyriadGUI and on modern
% (shader-based) OpenGL.
%
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

-type frame() :: gui:frame().
-type panel() :: gui:panel().
-type button() :: gui:button().
-type status_bar() :: gui:status_bar().

-type app_gui_state() :: gui_event:app_gui_state().
-type event_elements() :: gui_event:event_elements().
-type app_event_return() :: gui_event:app_event_return().


% Notably for the defaults:
-include("controller_defines.hrl").


% GUI section.


% Application-specific GUI information, to be stored in the app_specific_info
% field of the MyriadGUI app_gui_state record, and to be passed between event
% drivers.
%
-record( controller_gui_info, {

	% The main frame of this controller:
	main_frame :: frame(),

	% The main panel of this controller, containing the GL canvas:
	main_panel :: panel(),

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
				"host (no GLX visual reported), controller cannot be run." );

		GlxInfoStr ->
			case gui_opengl:is_hardware_accelerated( GlxInfoStr ) of

				true ->
					test_facilities:display( "Hardware acceleration is "
						"available, controller can be run." ),

					run_controller();

				false ->
					test_facilities:display( "No proper OpenGL support "
						"detected on host (no hardware acceleration reported), "
						"controller cannot be run." )

			end

	end.



run_controller() ->

	test_facilities:display( "This test will display a frame "
		"comprising a main 3D controller view, and controls on the right." ),

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
		{ get_main_window_width() div 2, get_main_window_height() div 2 },


	MainFrame = gui:create_frame( _Title="OSDL-Space Controller",
		_FramePos=auto, FrameSize, _FrameStyle=default, _Id=main_frame_id,
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

	GLBaseInfo = { GLCanvas, GLContext },


	gui:subscribe_to_events( { [ onResized, onShown, onWindowClosed ],
							   MainFrame } ),

	% Needed, otherwise if that frame is moved out of the screen or if another
	% windows overlaps, the OpenGL canvas gets garbled and thus must be redrawn:
	% (onResized a priori not needed)
	%
	gui:subscribe_to_events( { onRepaintNeeded, GLCanvas } ),

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

	QuitButtonId = quit_button_id,

	QuitButton = gui:create_button( "Quit", Position, ButtonSize, ButtonStyle,
									QuitButtonId, ButtonParent ),

	Buttons = [ QuitButton ],

	gui:subscribe_to_events( [ { onButtonClicked, B } || B <- Buttons ] ),


	%gui:set_tooltip( MainPanel, "Controls for the selectors" ),

	[ gui:add_to_sizer( ControlBoxSizer, B, expand_fully ) || B <- Buttons ],

	gui:set_sizer( SelectorPanel, ControlBoxSizer ),

	% Would prevent the panel to receive key presses:
	%RenderBoxSizer = gui:create_sizer_with_labelled_box( vertical, MainPanel,
	%                                                     "World rendering" ),

	% Same:
	%RenderBoxSizer = gui:create_sizer_with_box( vertical, MainPanel ),

	% Only one preserving key presses:
	RenderBoxSizer = gui:create_sizer( vertical ),

	%gui:subscribe_to_events( { onKeyPressed, Canvas } ),

	%gui:set_background_color( Canvas, red ),

	%gui:clear( Canvas ),

	gui:add_to_sizer( RenderBoxSizer, GLCanvas,
					  [ { proportion, 1 }, expand_fully ] ),

	%gui:set_tooltip( Canvas, "Rendering of OSDL-Space." ),

	gui:set_sizer( MainPanel, RenderBoxSizer ),

	gui:set_sizer( MainFrame, MainSizer ),

	% Focus needed to receive events; both components work:
	gui:set_focus( MainPanel ),
	%gui:set_focus( GLCanvas ),

	% Sets the GUI to visible:
	gui:show( MainFrame ),

	% FIXME
	Screen = #screen{ center={ get_main_window_width() / 3 - 550,
							   get_main_window_height() / 2 } },

	CtlrSpecificGUIInfo = #controller_gui_info{ main_frame=MainFrame,
												main_panel=MainPanel,
												quit_button=QuitButton,
												status_bar=StatusBar,
												screen=Screen },


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
			window_closed ] } ], GLBaseInfo, CtlrSpecificGUIInfo ),

	% Overrides the default drivers with ours:
	DrvAppGUIState = gui_event:set_event_drivers( [
		{ onShown,         fun ctrl_onShown_driver/2 },
		{ onRepaintNeeded, fun ctrl_onRepaintNeeded_driver/2 },
		{ onResized,       fun ctrl_onResized_driver/2 },
		{ onWindowClosed,  fun ctrl_onWindowClosed/2 } ],
								 InitAppGUIState ),

	% Wanting to catch up with the computations:
	erlang:process_flag( priority, _Level=high ),

	gui:push_status_text( "Initialised.", StatusBar ),

	% OpenGL will be initialised only when the corresponding frame will be ready
	% (that is once first reported as resized):
	%
	gui_main_loop( DrvAppGUIState ).



% @doc The main loop of this test, driven by the receiving of MyriadGUI
% messages, whose events are converted to application ones.
%
% OpenGL will be initialised only when the corresponding frame will be ready
% (that is once first reported as resized).
%
-spec gui_main_loop( app_gui_state() ) -> no_return().
gui_main_loop( ok ) ->
	throw( a1 );

gui_main_loop( AppGUIState ) ->

	% Triggered whenever a user event is received and can be promoted to an
	% application event; blocking version:
	%
	case gui_event:get_application_event( AppGUIState ) of

		{ { quit_requested, _BaseEvent }, QuitAppGUIState } ->
			on_quit( QuitAppGUIState );

		{ _MaybeAppEventPair=undefined, _EventAppGUIState=undefined } ->
			throw( eeee );

		{ _MaybeAppEventPair=undefined, EventAppGUIState } ->
			% User event processed without generating an application one:
			% (we could trigger a rendering from there as well)
			%
			gui_main_loop( EventAppGUIState );

		% Security:
		{ { OtherAppEvent, _BaseEvent }, OtherAppGUIState } ->
			trace_utils:warning_fmt( "Unhandled (hence ignored) application "
				"event by this controller:~n~ts.",
				[ gui_event:application_event_to_string( OtherAppEvent ) ] ),
			gui_main_loop( OtherAppGUIState );

		undefined ->
			% As this GUI is to be updated even in the absence of user actions:
			case AppGUIState#app_gui_state.opengl_base_state of

				{ uninitialised, _GLCanvas, _GLContext } ->
					trace_utils:debug( "(OpenGL not initialised yet)" ),
					gui_main_loop( AppGUIState );

				{ initialised, _GLCanvas, _GLContext } ->
					RenderAppGUIState = update_rendering( AppGUIState ),
					gui_main_loop( RenderAppGUIState )

			end;

		% Extra safety:
		OtherAppEventReturn ->
			trace_utils:error_fmt( "Unexpected (hence ignored) application "
				"event return received by this controller (abnormal):~n~p.",
				[ OtherAppEventReturn ] ),
			throw( { invalid_app_event_return, OtherAppEventReturn } )

	end.



% @doc The application-specific event driver for the onShown (user) event type,
% overriding default_onShown_driver/2: sets up OpenGL, once for all, now that a
% proper OpenGL context is available.
%
% Its type is event_driver().
%
% The most suitable first location to initialise OpenGL, as making a GL context
% current requires a shown window.
%
-spec ctrl_onShown_driver( event_elements(), app_gui_state() ) ->
								app_event_return().
% Here OpenGL is to be used, but is not initialised yet.
%
% This is the most suitable first location to initialise OpenGL, as making a GL
% context current requires a shown window. So, as soon as the main frame is
% shown, OpenGL is initialised once from all, and the onShown event is not
% listened to anymore (anyway it is not expected to be triggered again).
%
% As a result, this is the only clause defined: neither GLStatus=initialised nor
% a disabled OpenGL base state are to support.
%
ctrl_onShown_driver( _Elements=[ Frame, FrameId, EventContext ],
		AppGUIState=#app_gui_state{
			opengl_base_state={ _GLStatus=uninitialised, _GLCanvas,
								_GLContext } } ) ->

	% A transient former content for frame and canvas can be seen briefly, as we
	% did not succeed in clearing it early at start-up.

	trace_utils:debug_fmt( "Frame ~ts (ID: ~ts) is shown (~ts), with an "
		"initial size of ~w; using OpenGL, which as expected is not "
		"initialised yet; initialising it.",
		[ gui:object_to_string( Frame ), gui_id:id_to_string( FrameId ),
		  gui:context_to_string( EventContext ), gui:get_size( Frame ) ] ),

	% Optional yet better:
	gui:unsubscribe_from_events( { onShown, Frame } ),

	% Done once for all:
	InitAppGUIState = initialise_opengl( AppGUIState ),

	{ _MaybeAppEventPair=undefined, InitAppGUIState }.



% @doc Sets up OpenGL, once for all, once a proper OpenGL context is available.
%
% Defined for separation of concern, even if called from a single location.
%
-spec initialise_opengl( app_gui_state() ) -> app_gui_state().
initialise_opengl( AppGUIState=#app_gui_state{
		% Check:
		opengl_base_state={ uninitialised, GLCanvas, GLContext },
		app_specific_info=#controller_gui_info{ } } ) ->

	% Initial size of canvas is typically 20x20 pixels:
	trace_utils:debug_fmt(
		"Initialising OpenGL (whereas canvas is of initial size ~w).",
		[ gui:get_size( GLCanvas ) ] ),

	% So done only once:
	gui_opengl:set_context_on_shown( GLCanvas, GLContext ),

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

	InitAppGUIState = AppGUIState#app_gui_state{
		opengl_base_state={ initialised, GLCanvas, GLContext } },

	% As the initial onResized was triggered whereas no OpenGL state was
	% already available:
	%
	on_main_frame_resized( InitAppGUIState ).



% Overrides default_onRepaintNeeded_driver/2:
ctrl_onRepaintNeeded_driver(
		_Elements=[ _GLCanvas, _GLCanvasId, _EventContext ],
		AppGUIState=#app_gui_state{
			opengl_base_state={ _GLStatus=uninitialised, _SecondGLCanvas,
								_SecondGLContext } } ) ->

	%trace_utils:debug_fmt( "GL canvas ~w to be repainted, "
	%   "however OpenGL is not initialised yet.", [ GLCanvas ] ),

	{ _MaybeAppEventPair=undefined, AppGUIState };


ctrl_onRepaintNeeded_driver( _Elements=[ GLCanvas, _GLCanvasId, _EventContext ],
		AppGUIState=#app_gui_state{
			opengl_base_state={ _GLStatus=initialised, SameGLCanvas,
								_GLContext },
			app_specific_info=_CtrlSpecificInfo } ) ->

	%trace_utils:debug_fmt( "GL canvas ~w to be repainted, "
	%   "which can be done as OpenGL is already initialised.", [ GLCanvas ] ),

	basic_utils:assert_equal( GLCanvas, SameGLCanvas ),

	% A rendering is not strictly necessary in this case, as anyway a regular
	% redraw is to happen soon afterwards.

	gui:enable_repaint( GLCanvas ),

	% Includes the GL flushing and the buffer swaping:
	%
	% (no extra element needed, and no change operated on this term)
	%
	%render( TestSpecificInfo ),

	{ _MaybeAppEventPair=undefined, AppGUIState }.



% @doc The controller-specific event driver for the onResized (user) event type.
%
% Its type is event_driver().
%
-spec ctrl_onResized_driver( event_elements(), app_gui_state() ) ->
											app_event_return().
ctrl_onResized_driver( _Elements=[ _ParentWindow, _ParentWindowId,
								   NewParentSize, _EventContext ],
		AppGUIState=#app_gui_state{
			opengl_base_state={ uninitialised, GLCanvas, _GLContext } } ) ->

	% For a window, the first resizing event happens (just) before its onShown
	% one: not ready yet (first onResized, before onShown).

	trace_utils:debug_fmt( "GL canvas ~w to be resized to ~w, "
		"however OpenGL is not initialised yet.", [ GLCanvas, NewParentSize ] ),

	{ _MaybeAppEventPair=undefined, AppGUIState };


ctrl_onResized_driver( _Elements=[ _ParentWindow, _ParentWindowId,
								   _NewParentSize, _EventContext ],
		AppGUIState=#app_gui_state{
			opengl_base_state={ initialised, _GLCanvas, _GLContext } } ) ->

	ResizedAppGUIState = on_main_frame_resized( AppGUIState ),

	{ _MaybeAppEventPair=undefined, ResizedAppGUIState }.



ctrl_onWindowClosed( Elements=[ ParentWindow, _ParentWindowId,
								_EventContext ],
					 AppGUIState ) ->

	trace_utils:info_fmt( "Main frame ~w closed, controller terminating.",
						  [ ParentWindow ] ),

	% Very final check, while there is still an OpenGL context:
	gui_opengl:check_error(),

	gui:destruct_window( ParentWindow ),

	BaseGUIEvent = { onWindowClosed, Elements },

	AppEventPair = { quit_requested, BaseGUIEvent },

	{ AppEventPair, AppGUIState }.



% Section for helpers:

% @doc Managing a resizing of the main frame.
%
% Defined as a separate function, as to be called from two contexts: when the
% main frame is shown and when a resizing is needed.
%
% OpenGL context expected here to have already been set.
%
-spec on_main_frame_resized( app_gui_state() ) -> app_gui_state().
on_main_frame_resized( GUIState=#app_gui_state{
		opengl_base_state={ initialised, GLCanvas, _GLContext },
		app_specific_info=#controller_gui_info{ main_panel=MainPanel } } ) ->
	% Maximises widgets in their respective area:

	% First, panel in main frame:
	gui:maximise_in_parent( MainPanel ),

	% Then OpenGL canvas in panel:
	{ CanvasWidth, CanvasHeight } = gui:maximise_in_parent( GLCanvas ),

	%trace_utils:debug_fmt( "New client canvas size: {~B,~B}.",
	%                       [ CanvasWidth, CanvasHeight ] ),

	% Lower-left corner and size of the viewport in the current window:
	gl:viewport( 0, 0, CanvasWidth, CanvasHeight ),

	% Apparently, at least on a test setting, a race condition (discovered
	% thanks to the commenting-out of a debug trace) seems to exist between the
	% moment when the canvas is resized and the one when a new OpenGL rendering
	% is triggered afterwards; the cause is probably that maximising involves an
	% (Erlang) asynchronous message to be sent from this user process and to be
	% received and applied by the process of the target window, whereas a GL
	% (NIF-based) operation is immediate; without a sufficient delay, the
	% rendering will thus take place according to the former (e.g. minimised)
	% canvas size, not according to the one that was expected to be already
	% resized.
	%
	gui:sync( GLCanvas ),

	gl:matrixMode( ?GL_PROJECTION ),

	gl:loadIdentity(),

	Left = -2.0,
	Bottom = -2.0 * CanvasHeight / CanvasWidth,
	Near = -20.00,
	gl:ortho( Left, _Right=-Left, Bottom, _Top=-Bottom, Near, _Far=-Near ),

	gl:matrixMode( ?GL_MODELVIEW ),
	gl:loadIdentity(),

	cond_utils:if_defined( myriad_check_opengl_support,
						   gui_opengl:check_error() ),

	% Includes the swapping of buffers:
	update_rendering( GUIState ).



% @doc Updates the rendering.
%
% Expected to be called periodically.
%
-spec update_rendering( app_gui_state() ) -> app_gui_state().
update_rendering( GUIState=#app_gui_state{
		app_specific_info=#controller_gui_info{} } ) ->
	render( GUIState ).


% @doc Performs a ("pure OpenGL") rendering, based on the specified GUI
% information.
%
-spec render( app_gui_state() ) -> app_gui_state().
render( GUIState=#app_gui_state{} ) ->
	GUIState.


% Called whenever having to quit.
on_quit( #app_gui_state{ app_specific_info=#controller_gui_info{
												main_frame=MainFrame } } ) ->

	trace_utils:debug( "Quit requested." ),

	gui:destruct_window( MainFrame ),

	% Simply stop recursing.

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
