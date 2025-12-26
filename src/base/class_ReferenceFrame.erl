% Copyright (C) 2023-2026 Olivier Boudeville
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
% Creation date: Saturday, June 3, 2023.

-module(class_ReferenceFrame).

-moduledoc """
A **most general frame of reference** lightweight base class.

See also Myriad's `reference_frame` module, used internally here.
""".


-define( class_description,
         "Lightweight base class for frames of reference of all sorts." ).


% Determines what are the direct mother classes of this class (if any):
%
% (no even a trace emitter per se)
%
-define( superclasses, [] ).


% Design notes:
%
% A frame of reference not having a parent one is thus expressed in a canonical,
% absolute, most general one.
%
% At this level, no specific dimensions apply. So for example we deal with
% points in a space of arbitrary dimension, not 2D or 3D points.
% Child classes will further refine this.


% Describes the class-specific attributes:
-define( class_attributes, [

    % Finally not defined specifically: each child class is to store such
    % information by itself, typically in a transition matrix of its own.
    %
    %{ origin, point(),
    %  "the origin of this frame of reference, expressed in its parent one, "
    %  "if any, otherwise in absolute terms" },

    { parent, option( any_ref_frame() ), "the parent frame of reference "
      "(if any; and of any type) of this one" } ] ).



-doc """
A reference frame, as a mere term (rather than as a WOOPER instance, that is a
process).

Type name chosen not to collide with the type of Myriad's `reference_frame`
module.
""".
-type ref_frame() :: wooper:passive_instance().



-doc "A reference frame, as a WOOPER process.".
-type ref_frame_pid() :: wooper:instance_pid().



-doc """
A reference frame, as designated by an identifier, defined as a (positive)
integer, referring (as a key) to an (implicit) table of reference frames.
""".
-type ref_frame_id() :: count().



-doc "Any type of reference onto a reference frame.".
-type any_ref_frame() :: ref_frame()
                       | ref_frame_pid()
                       | ref_frame_id().


-export_type([ ref_frame/0, ref_frame_pid/0, ref_frame_id/0,
               any_ref_frame/0 ]).



% Helper (non-static) functions:
-export([ ref_frame_to_string/1, to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").



% Implementation notes:
%
% "referential" is not a proper choice of word here.


% Type shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type point() :: point:point().



-doc """
Constructs a blank reference frame, with no parent defined.
""".
construct( State ) ->
    setAttribute( State, parent, undefined ).



%-spec construct( wooper:state(), point() ) -> wooper:state().
% Not appropriate as construct/3 is not either:
%construct( State, Origin ) ->
%   construct( State, Origin, _ParentRefFrame=undefined ).


% @doc Constructs an absolute reference frame centered at the specified origin.
%-spec construct( wooper:state(), point() ) -> wooper:state().
% Not appropriate as construct/3 is not either:
%construct( State, Origin ) ->
%   construct( State, Origin, _ParentRefFrame=undefined ).


% @doc Constructs a reference frame relative to the specified parent one, and
% centered at the specified origin of it.
%
%-spec construct( wooper:state(), point(), option( any_ref_frame() ) ) ->
%                                               wooper:state().
% Not appropriate: cannot access the data of child class at this point:
%construct( State, Origin, MaybeParentRefFrame ) ->
%   OrgState = executeOneway( State, setOrigin, Origin ),
%   setAttribute( OrgState, parent, MaybeParentRefFrame ).



% Request section.


-doc """
Returns the origin of this reference frame relatively to its parent if any,
otherwise absolutely.
""".
-spec getOrigin( wooper:state() ) -> const_request_return( point() ).
getOrigin( _State ) ->
    throw( not_overridden ).




% Oneway section.


-doc """
Sets the origin of this reference frame relatively to its parent if any,
otherwise absolutely.
""".
-spec setOrigin( wooper:state(), point() ) -> oneway_return().
setOrigin( _State, _Origin ) ->
    throw( not_overridden ).




% Static section.



-doc """
Checks (with necessary yet not sufficient conditions) that the specified term is
a reference frame; if yes, returns it, otherwise throws an exception.
""".
-spec check_ref_frame( term() ) -> static_return( any_ref_frame() ).
check_ref_frame( RefPassivInstState=#state_holder{} ) ->
    wooper:return_static(
        wooper:check_instance_of( _Classname=?MODULE, RefPassivInstState ) );

check_ref_frame( RefPid ) when is_pid( RefPid ) ->
    wooper:return_static( RefPid );

check_ref_frame( RefId ) when is_integer( RefId ) ->
    wooper:return_static( RefId );

check_ref_frame( Other ) ->
    throw( { invalid_ref_frame, Other } ).



-doc """
Checks (with necessary yet not sufficient conditions) that the specified term is
a maybe-reference frame; if yes, returns it, otherwise throws an exception.
""".
-spec check_maybe_ref_frame( term() ) -> static_return( any_ref_frame() ).
check_maybe_ref_frame( MaybeRef=undefined ) ->
    wooper:return_static( MaybeRef );

check_maybe_ref_frame( Ref ) ->
    wooper:return_static( check_ref_frame( Ref ) ).



-doc """
Returns a textual description of the specified reference frame of any type.
""".
-spec ref_frame_to_string( any_ref_frame() ) -> ustring().
ref_frame_to_string( PassiveRefFrame=#state_holder{} ) ->
    wooper:execute_const_request( PassiveRefFrame, toString );

ref_frame_to_string( RefFramePid ) when is_pid( RefFramePid ) ->
    % Not executing a request for that:
    text_utils:format( "reference frame ~w", [ RefFramePid ] );

ref_frame_to_string( RefFrameId ) ->
    text_utils:format( "reference frame #~B", [ RefFrameId ] ).



-doc "Returns a textual description of this reference frame.".
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

    ParentStr = case ?getAttr(parent) of

        undefined ->
            "absolute reference frame";

        AnyParentRef ->
            text_utils:format( "reference frame whose parent one is ~ts",
                               [ ref_frame_to_string( AnyParentRef ) ] )

    end,

    text_utils:format( "~ts whose origin is ~ts",
                       [ ParentStr, point:to_string( ?getAttr(origin) ) ] ).
