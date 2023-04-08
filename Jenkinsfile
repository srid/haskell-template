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
                    # Sandbox must be disabed for:
                    # https://github.com/srid/haskell-flake/issues/21
                    nix \
                        --option sandbox false \
                        flake check -L \
                        --override-input systems github:nix-systems/x86_64-linux
                   '''
            }
        }
    }
}
