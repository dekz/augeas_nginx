module Nginx =
  autoload xfm
  let block_re = "http" | "events"
               | "upstream"
               | "if"
  let block_seq_re = "server" | "location"

  let identifier = /[a-zA-Z0-9_-]+/

  let empty =
            Util.empty
          | Util.comment

  let eol = Util.eol
  let sp = Sep.space

  let simple =
      let kw = identifier - block_re
    in let sto = store /[^ \t\n;][^;]*/ . Util.del_str ";"
    in Build.key_value_line_comment
       kw sp sto Util.comment

  let rec base_block =
       let entry = Util.indent . (simple | base_block)
    in [ key block_re
       . Build.block_newlines entry Util.comment
       . Util.eol ]

  let server_block =
       let entry = Util.indent . (simple | base_block)
    in let kw = key block_seq_re
    in [ kw
       . Build.block_newlines entry Util.comment
       . Util.eol ]

  let server_blocks = [seq "server" . server_block+ ]
  let sequential_blocks = server_blocks

  (* Outer generic blocks like http which can contain more blocks *)
  let rec block =
       let entry = Util.indent . (simple | sequential_blocks | block)
    in [ key block_re
       . Build.block_newlines entry Util.comment
       . Util.eol ]


  let blocks = block
  let defined = block | simple

  (* View: lns *)
  let lns = ( empty | defined )*

  (* Variable: filter *)
  let filter = incl "/etc/nginx/nginx.conf"
             . incl "/usr/portage/www-servers/nginx/files/nginx.conf"

  let xfm = transform lns filter
