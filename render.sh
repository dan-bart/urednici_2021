quarto render szu-analyza-stsl.qmd --output index.html --output-dir web_partial
netlify deploy --dir web_partial --site curious-profiterole-ba0eee
netlify deploy --dir web_partial --site curious-profiterole-ba0eee --prod

# https://curious-profiterole-ba0eee.netlify.app
