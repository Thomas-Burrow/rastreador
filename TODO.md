# Etapas para serem realizadas no futuro
- Escrever as historias (estórias?) de usuário e coisas deste tipo
- Gerar diagramas UML (case de uso, estados possiveis para veiculo, etc...)
- Authn/Authz, idealmente conectando com SSO (Login Unico) se existir no local
- Implementar RBAC e criar pagina para gerenciar os Roles para usuarios
- Escrever mais documentação
- Preparar uma historia para deployment (dockerfile, mover settings.yml para settings.yml.example e adicionar settings.yml para .gitignore para diminuir a chance de vazar segredos com um git add errado)
- Gravar eventos para um outro table e página com linha do tempo para ordem de serviço
- Gravar timestamp de ultima edição quando veiculo for marcado como retirado, permitindo movimento de entradas velhas para arquivamento ou deleção após passar um periodo de retenção
- Provavelmente mudar o DB de SQLite para Postgres para ter um sistema mais sério
