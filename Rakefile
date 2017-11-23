require "tmpdir"

desc "Remove the output directory"
task :clean do
  rm_rf "output"
end

desc "Generate all the files for the website (development)"
task build: [:clean] do
  sh "bundle exec nanoc compile"
  sh "npm run build"
end

desc "Generate all the files for the website (production)"
task build_prod: [:clean] do
  sh "bundle exec nanoc compile --env=prod"
  sh "npm run build:prod"
end

desc "Deploy the website to GitHub pages"
task deploy: [:build_prod] do
  Dir.mktmpdir do |dir|
    sh "git worktree prune"
    sh "git worktree add #{dir} gh-pages"
    sh "cp -r output/* #{dir}"
    Dir.chdir(dir) do
      sh "git add ."
      sh "git commit -m \"Site updated to $(git log --pretty=\"%h\" -n1 master)\""
      sh "git push"
    end
  end
end
