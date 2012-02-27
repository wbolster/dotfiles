if exists('g:no_php_conceal') || !has('conceal') || &enc != 'utf-8'
    finish
endif

syntax match phpNiceOperator "||" conceal cchar=∨ contained containedin=phpRegion
syntax match phpNiceOperator "&&" conceal cchar=∧ contained containedin=phpRegion
syntax match phpNiceOperator "!" conceal cchar=¬ contained containedin=phpRegion
syntax match phpNiceOperator "<=" conceal cchar=≤ contained containedin=phpRegion
syntax match phpNiceOperator ">=" conceal cchar=≥ contained containedin=phpRegion
syntax match phpNiceOperator "==" conceal cchar=≈ contained containedin=phpRegion
syntax match phpNiceOperator "===" conceal cchar=≡ contained containedin=phpRegion
syntax match phpNiceOperator "::" conceal cchar=∷ contained containedin=phpRegion
syntax match phpNiceOperator "!=" conceal cchar=≠ contained containedin=phpRegion
syntax match phpNiceRelation "=>" conceal cchar=⇛ contained containedin=phpRegion
syntax match phpNiceMemberSelector "\->" conceal cchar=→ contained containedin=phpRegion

hi link phpNiceOperator phpOperator
hi link phpNiceStatement phpStatement
hi link phpNiceKeyword phpKeyword
hi link phpNiceRelation phpRelation
hi link phpNiceMemberSelector phpMemberSelector
hi! link Conceal phpOperator

setlocal conceallevel=2
