# comandos para internacionalizacao

## Criando os diretorios para .po
tools::update_pkg_po()

## Checagem de arquivos po
tools::checkPoFile()
tools::checkPoFiles()

## Extrai mensagens traduzidas
tools::xgettext()
tools::xgettext2pot()
tools::xngettext()

## Funcoes para traducao
base::gettext()
base::ngettext()
base::bindtextdomain()

# Inserir no prompt de comando:
# msgfmt -c --statistics -o inst/po/pt_BR/LC_MESSAGES/R-leem.mo po/R-pt_BR.po
