git init
git remote remove origin
git remote add origin git@github.com:PhDMeiwp/ggradar2.git
git add .
git commit -m "Depends: R version >= 4.3.2"
git push origin master -f
