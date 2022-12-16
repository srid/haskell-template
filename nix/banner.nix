{ wrapperName, wrapper, ... }:

''
  function menu () {
    echo
    echo -e "\033[1;31m### ️🔨 Welcome to Nix devshell ###\n\033[0m"
    ${wrapper}/bin/${wrapperName}
    echo
    echo "(Run '${wrapperName}' to display this menu again)"
    echo
  }
  menu
''
