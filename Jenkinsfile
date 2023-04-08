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
                    echo '["x86_64-linux"]' > .git/systems.nix
                    # Sandbox must be disabed for:
                    # https://github.com/srid/haskell-flake/issues/21
                    nix \
                        --option sandbox false \
                        flake check -L \
                        --override-input systems path:.git/systems.nix
                   '''
            }
        }
    }
}
