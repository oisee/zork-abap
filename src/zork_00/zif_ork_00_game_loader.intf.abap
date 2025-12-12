*&---------------------------------------------------------------------*
*& Interface ZIF_ORK_00_GAME_LOADER
*& Load Z-Machine game story files
*&---------------------------------------------------------------------*
INTERFACE zif_ork_00_game_loader PUBLIC.

  TYPES: BEGIN OF ts_game_info,
           id          TYPE string,
           description TYPE string,
           filesize    TYPE i,
           version     TYPE string,
           filename    TYPE string,
         END OF ts_game_info,
         tt_game_list TYPE STANDARD TABLE OF ts_game_info WITH EMPTY KEY.

  METHODS:
    " List available games
    list_games
      RETURNING VALUE(rt_list) TYPE tt_game_list,

    " Load game by ID (optional - file loaders may use constructor path)
    load
      IMPORTING iv_id           TYPE string OPTIONAL
      RETURNING VALUE(rv_story) TYPE xstring.

ENDINTERFACE.
