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
% Creation date: Sunday, June 18, 2023.


% Defines for the OSDL-Space controller.


% Expected to be already included.
%
% For keyboard defines; the sole include that MyriadGUI user code shall
% reference:
%
%-include_lib("myriad/include/myriad_gui.hrl").


% Default key mappings:

-define( default_help_scancode, ?MYR_K_h ).

-define( default_fullscreen_keycode, ?MYR_K_f ).

-define( default_quit_keycode, ?MYR_K_q ).
-define( default_quit_scancode, ?MYR_SCANCODE_ESCAPE ).
