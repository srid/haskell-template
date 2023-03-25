pipeline {
    agent any
    stages {
        stage ('Nix build') {
            steps {
                sh 'nix build'
            }
        }
        stage ('Nix develop shell') {
            steps {
                sh 'nix develop -c echo'
            }
        }
        stage ('Flake checks') {
            steps {
                sh '''
                    # Because 'nix flake check' is not system-aware
                    # See https://srid.ca/haskell-template/checks
                    nix run nixpkgs#sd \
                        'systems = nixpkgs.lib.systems.flakeExposed' \
                        'systems = [ "x86_64-linux" ]' \
                        flake.nix
                    # Sandbox must be disabed for:
                    # https://github.com/srid/haskell-flake/issues/21
                    nix \
                        --option sandbox false \
                        flake check -L                    
                   '''
            }
        }
    }
}
