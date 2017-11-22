require "tmpdir"

desc "Deploy the website to GitHub pages"
task :deploy do
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
