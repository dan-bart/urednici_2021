<script>
        document.getElementById('copy-link-graph_id').addEventListener('click', function(event) {
            event.preventDefault();  // Prevent default anchor behavior
            copyToClipboard('https://ideaapps.cerge-ei.cz/urednici_2024_single_page/graphs_long.html#graph_id');
        });

        function copyToClipboard(text) {
            navigator.clipboard.writeText(text).then(function() {
                $('#copy-link-modal').modal('show');
            }).catch(function(err) {
                console.error('Could not copy text: ', err);
            });
        }
</script>
