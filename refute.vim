" Vim syntax file
" Language:     WPCA
" Maintainer:   David Faitelson <david.faitelson@gmail.com>
" Last Change:  Mon May 21 20:36:10 IDT 2012

" Quit when a (custom) syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

" WPCA keywords
syn keyword     wpcaStatement   skip proc record theory
syn keyword     wpcaConditional	if fi
syn keyword     wpcaRepeat     	do od keeping
syn keyword     wpcaType     	int nat array of 
syn keyword     wpcaQuantifier 	all no some sum 
syn keyword     wpcaOperator 	and or not is new


hi def link 	wpcaStatement          	Statement
hi def link 	wpcaConditional        	Conditional
hi def link 	wpcaRepeat             	Repeat
hi def link 	wpcaType              	Type
hi def link	wpcaQuantifier		Statement		
hi def link	wpcaOperator 		Statement



