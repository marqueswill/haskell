Notação:
"=>" edição de arquivo
"==>" execução de comando no prompt do sistema operacional


** Passos principais (requerido no curso, feito pelo aluno)

1) estudar a sintaxe abstrata, ou seja, o(s) tipo(s) algébrico(s) com a estrutura da linguagem 
  => arquivo "AbsLI.hs"

2) definir/editar o typechecker com base na sintaxe abstrata 
  => arquivo "Typechecker.hs"

3) compilar o driver (main) do typechecker
  ==> ghc --make Typecheck.hs

4) testar o executável do typechecker com exemplos:
  ==> Typecheck < examples\ex1.li7

5) definir/editar o interpretador com base na sintaxe abstrata 
  => arquivo "Interpreter.hs"

6) compilar o driver (main) do interpretador 
  ==> ghc --make Interpret.hs

7) testar o executável do interpretador com exemplos:
  ==> Interpret < examples\ex1.li7
  

** Passos preliminares (feito pelo professor)
-4) Definir/editar a sintaxe concreta (feito pelo professor)
  ==> arquivo LI7.cf

-3) Gerar os fontes do analisador sintático (parser) e léxico (lexer), assim como o Makefile usando o BNF Converter
  ==> bnfc  -m  LI7.cf

-2) Compilar os fontes do parser e lexer 
  ==> make

-1) Definir o driver (main) do interpretador
  => arquivo "Interpret.hs"
  
0) Definir o driver (main) do checador de tipos (typechecker)
  => arquivo "Typecheck.hs"  
   

---------------------

Observações

-> O arquivo "AbsLI.hs" é gerado a partir do arquivo "LI7.cf". Assim, caso o último seja editado, o primeiro terá que ser gerado
novamente. Para fazer alterações desejadas no arquivo "AbsLI.hs", o mesmo não deve ser editado diretamente: 
deve-se alterar o "LI7.cf" e gerar o "AbsLI.hs" novamente usando o BNF Converter.

-> Para a execução dos "Passos Principais", é necessário ter a plataforma Haskell instalada.
https://www.haskell.org/platform/

-> Para a execução dos "Passos Preliminares", é necessário:
1) instalar o BNF Converter
http://bnfc.digitalgrammars.com/

2) Caso o sistema operacional não tenha nativamente o "make" (p.ex. Windows),
é necessário instalá-lo
http://www.steve.org.uk/Software/make/   