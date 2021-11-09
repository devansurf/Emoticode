
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'leftPLUSMINUSleftMULTIPLYDIVIDEDIVIDE EQUALS FLOAT INT LPAREN MINUS MULTIPLY NAME PLUS RPAREN\n    calc : expression\n         | var_assign\n         | empty\n    \n    var_assign : NAME EQUALS expression\n    \n    expression : expression PLUS expression\n               | expression DIVIDE expression\n               | expression MULTIPLY expression\n               | expression MINUS expression\n    \n    expression : NAME\n    \n    expression : INT\n               | FLOAT\n    \n    expression : LPAREN expression RPAREN\n    \n    empty :\n    '
    
_lr_action_items = {'NAME':([0,8,9,10,11,12,13,],[5,15,15,15,15,15,15,]),'INT':([0,8,9,10,11,12,13,],[6,6,6,6,6,6,6,]),'FLOAT':([0,8,9,10,11,12,13,],[7,7,7,7,7,7,7,]),'LPAREN':([0,8,9,10,11,12,13,],[8,8,8,8,8,8,8,]),'$end':([0,1,2,3,4,5,6,7,15,16,17,18,19,20,21,],[-13,0,-1,-2,-3,-9,-10,-11,-9,-5,-6,-7,-8,-4,-12,]),'PLUS':([2,5,6,7,14,15,16,17,18,19,20,21,],[9,-9,-10,-11,9,-9,-5,-6,-7,-8,9,-12,]),'DIVIDE':([2,5,6,7,14,15,16,17,18,19,20,21,],[10,-9,-10,-11,10,-9,10,-6,-7,10,10,-12,]),'MULTIPLY':([2,5,6,7,14,15,16,17,18,19,20,21,],[11,-9,-10,-11,11,-9,11,-6,-7,11,11,-12,]),'MINUS':([2,5,6,7,14,15,16,17,18,19,20,21,],[12,-9,-10,-11,12,-9,-5,-6,-7,-8,12,-12,]),'EQUALS':([5,],[13,]),'RPAREN':([6,7,14,15,16,17,18,19,21,],[-10,-11,21,-9,-5,-6,-7,-8,-12,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'calc':([0,],[1,]),'expression':([0,8,9,10,11,12,13,],[2,14,16,17,18,19,20,]),'var_assign':([0,],[3,]),'empty':([0,],[4,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> calc","S'",1,None,None,None),
  ('calc -> expression','calc',1,'p_emoticode','emoticode.py',72),
  ('calc -> var_assign','calc',1,'p_emoticode','emoticode.py',73),
  ('calc -> empty','calc',1,'p_emoticode','emoticode.py',74),
  ('var_assign -> NAME EQUALS expression','var_assign',3,'p_var_assign','emoticode.py',80),
  ('expression -> expression PLUS expression','expression',3,'p_expression','emoticode.py',86),
  ('expression -> expression DIVIDE expression','expression',3,'p_expression','emoticode.py',87),
  ('expression -> expression MULTIPLY expression','expression',3,'p_expression','emoticode.py',88),
  ('expression -> expression MINUS expression','expression',3,'p_expression','emoticode.py',89),
  ('expression -> NAME','expression',1,'p_expression_var','emoticode.py',97),
  ('expression -> INT','expression',1,'p_expression_int_float','emoticode.py',104),
  ('expression -> FLOAT','expression',1,'p_expression_int_float','emoticode.py',105),
  ('expression -> LPAREN expression RPAREN','expression',3,'p_expression_parenthesis','emoticode.py',112),
  ('empty -> <empty>','empty',0,'p_empty','emoticode.py',122),
]
