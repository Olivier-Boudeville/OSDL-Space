% Copyright (C) 2023-2025 Olivier Boudeville
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
% Creation date: Sunday, June 4, 2023.

-module(class_3DReferential).

-moduledoc "A **3D frame of reference** class.".


-define( class_description,
         "Lightweight base class for 3D frames of reference." ).


% Determines what are the direct mother classes of this class (if any):
-define( superclasses, [ class_ReferenceFrame ] ).



% Describes the class-specific attributes:
%
% (note that we do not introduce new attributes, we override their description)
%
-define( class_attributes, [

    % Finally directly stored in its transition matrix:
    %{ origin, point3(),
    %  "the origin of this frame of reference, expressed in its parent one, "
    %  "if any, otherwise in absolute terms" },

    % Overriding inherited attribute:
    { parent, option( any_referential3() ),
      "the parent 3D frame of reference (if any; and of any type) "
      "of this one" },

    { to_parent, transition_matrix4(),
      "the transition matrix from this 3D frame of reference to its parent; "
      "if this frame of reference is absolute, then it is identity_4" } ] ).



-doc """
A 3D frame of reference, as a mere term (rather than as a WOOPER instance,
i.e. a process).
""".
-type ref_frame3() :: wooper:passive_instance().


-doc "A 3D frame of reference, as a WOOPER process.".
-type ref_frame3_pid() :: wooper:instance_pid().



-doc """
A 3D frame of reference, as designated by an identifier, defined as a (positive)
integer, referring (as a key) to an (implicit) table of 3D frame of references.
""".
-type ref_frame3_id() :: count().


-doc "Any type of reference onto a 3D frame of reference.".
-type any_ref_frame3() :: ref_frame3()
                        | ref_frame3_pid()
                        | ref_frame3_id().



-export_type([ ref_frame3/0, ref_frame3_pid/0, ref_frame3_id/0,
               any_ref_frame3/0 ]).


% Helper (non-static) functions:
-export([ ref_frame3_to_string/1, to_string/1 ]).


% Allows to define WOOPER base variables and methods for that class:
-include_lib("wooper/include/wooper.hrl").


% Implementation notes:
%
% Transformations (hence bidirectional) could be used instead of transition
% matrices (unidirectional).



% Type shorthands:

-type count() :: basic_utils:count().

-type ustring() :: text_utils:ustring().

-type point3() :: point3:point3().

-type vector3() :: vector3:vector3().
-type unit_vector3() :: vector3:unit_vector3().

%-type transition_matrix4() :: matrix4:transition_matrix4().



-doc """
Constructs an absolute 3D frame of reference, centered at the specified origin.
""".
-spec construct( wooper:state(), point3() ) -> wooper:state().
construct( State, Origin ) ->

    % Not class_ReferenceFrame:construct(State, Origin), as no matrix to store
    % it yet.

    NoParentState = class_ReferenceFrame:construct( State ),
    ParentTransMat4 = matrix4:translation( Origin ),

    setAttribute( NoParentState, to_parent, ParentTransMat4 ).



-doc """
Constructs a 3D frame of reference centered at the specified origin, relatively
to any specified one.
""".
-spec construct( wooper:state(), point3(), option( any_ref_frame3() ) ) ->
                        wooper:state().
construct( State, Origin, MaybeParent3DRefFrame ) ->
    NoParentState = class_ReferenceFrame:construct( State ),

    ParentTransMat4 = matrix4:translation( Origin ),

    setAttributes( NoParentState, [ { parent, MaybeParent3DRefFrame },
                                    { to_parent, ParentTransMat4 } ] ).



-doc """
Constructs a 3D frame of reference centered at the specified origin relatively
to the specified one, with its corresponding axes, which are supposed to be
already unit vectors.
""".
-spec construct( wooper:state(), point3(), unit_vector3(), unit_vector3(),
                 unit_vector3(), option( any_ref_frame3() ) ) -> wooper:state().
construct( State, Origin, X, Y, Z, MaybeParentReferential ) ->

    NoParentState = class_ReferenceFrame:construct( State ),

    cond_utils:if_defined( osdl_space_debug_referentials,
        begin
            point3:check( Origin ),
            vector3:check_unit_vectors( [ X, Y, Z ] ),
            vector3:check_orthogonal( X, Y ),
            vector3:check_orthogonal( X, Z ),
            vector3:check_orthogonal( Y, Z ),
            check_maybe_ref_frame( MaybeParentReferential )
        end ),

    % From this frame of reference to the parent one, the origin and axes of
    % this frame of reference being expressed in its parent one:
    %
    ParentTransMat4 = matrix4:transition( Origin, X, Y, Z ),

    setAttributes( NoParentState, [ { parent, MaybeParentReferential },
                                    { to_parent, ParentTransMat4 } ] ).



-doc """
Constructs a 3D frame of reference centered at the specified origin relatively
to the specified one, with the corresponding two X and Y axes, which are not
necessarily already unit vectors. The third axis, Z, will be deduced from X and
Y.
""".
-spec construct( wooper:state(), point3(), vector3(), vector3(),
                 option( any_ref_frame3() ) ) -> wooper:state().
construct( State, Origin, X, Y, MaybeParentReferential ) ->

    NoParentState = class_ReferenceFrame:construct( State ),

    cond_utils:if_defined( osdl_space_debug_referentials,
        begin
            point3:check( Origin ),
            vector3:check_vectors( [ X, Y ] ),
            check_maybe_ref_frame( MaybeParentReferential )
        end ),

    Xunit = vector3:normalise( X ),
    Yunit = vector3:normalise( Y ),
    Zunit = vector3:cross_product( X, Y ),

    % From this frame of reference to the parent one, the origin and axes of
    % this frame of reference being expressed in its parent one:
    %
    ParentTransMat4 = matrix4:transition( Origin, Xunit, Yunit, Zunit ),

    setAttributes( NoParentState, [ { parent, MaybeParentReferential },
                                    { to_parent, ParentTransMat4 } ] ).



% Request section.


-doc """
Returns the origin of this 3D frame of reference relatively to its parent if
any, otherwise absolutely.
""".
-spec getOrigin( wooper:state() ) -> const_request_return( point3() ).
getOrigin( State ) ->
    ParentTransMat4 = ?getAttr(to_parent),
    wooper:const_return_result( matrix4:get_column_o( ParentTransMat4 ) ).




% Oneway section.


-doc """
Sets the origin of this frame of reference, relatively to its parent if any,
otherwise absolutely.
""".
-spec setOrigin( wooper:state(), point3() ) -> oneway_return().
setOrigin( State, Origin ) ->
    NewParentTransMat4 = matrix4:set_column_o( ?getAttr(to_parent), Origin ),
    NewState = setAttribute( State, to_parent, NewParentTransMat4 ),
    wooper:return_state( NewState ).




% Static section.



-doc """
Checks (with necessary yet not sufficient conditions) that the specified term is
a (3D) frame of reference; if yes, returns it, otherwise throws an exception.
""".
-spec check_ref_frame( term() ) -> static_return( any_ref_frame3() ).
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
a maybe - 3D frame of reference; if yes, returns it, otherwise throws an
exception.
""".
-spec check_maybe_ref_frame( term() ) -> static_return( any_ref_frame3() ).
check_maybe_ref_frame( MaybeRef=undefined ) ->
    wooper:return_static( MaybeRef );

check_maybe_ref_frame( Ref ) ->
    wooper:return_static( check_ref_frame( Ref ) ).



-doc """
Returns a textual description of the specified frame of reference of any type.
""".
-spec ref_frame3_to_string( any_ref_frame3() ) -> ustring().
ref_frame3_to_string( PassiveRef_Frame=#state_holder{} ) ->
    wooper:execute_const_request( PassiveRef_Frame, toString );

ref_frame3_to_string( Ref_FramePid ) when is_pid( Ref_FramePid ) ->
    % Not executing a request for that:
    text_utils:format( "3D reference frame ~w", [ Ref_FramePid ] );

ref_frame3_to_string( Ref_FrameId ) ->
    text_utils:format( "3D reference frame #~B", [ Ref_FrameId ] ).



-doc "Returns a textual description of this frame of reference.".
-spec to_string( wooper:state() ) -> ustring().
to_string( State ) ->

    ParentStr = case ?getAttr(parent) of

        undefined ->
            "absolute 3D reference frame";

        AnyParentRef ->
            text_utils:format( "3D reference frame whose parent one is ~ts",
                               [ ref_frame3_to_string( AnyParentRef ) ] )

    end,

    text_utils:format( "~ts whose origin is ~ts",
                       [ ParentStr, point:to_string( ?getAttr(origin) ) ] ).
