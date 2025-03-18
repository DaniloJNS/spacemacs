;;; Compiled snippets and support files for `ruby-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'ruby-mode
                     '(("subjc" "subject(:$1) { $2 }" "subject with custom name" nil nil nil "/home/danilo/.emacs.d/private/snippets/ruby-mode/rspec-mode/subjc" nil nil)
                       ("subj" "subject { $1 }" "subject" nil nil nil "/home/danilo/.emacs.d/private/snippets/ruby-mode/rspec-mode/subj" nil nil)
                       ("letb" "let(:$1) do\n  $2\nend" "let block let(...) do ... end" nil nil nil "/home/danilo/.emacs.d/private/snippets/ruby-mode/rspec-mode/letb" nil nil)
                       ("let" "let(:$1) { $2 }" "let(..)  {...}" nil nil nil "/home/danilo/.emacs.d/private/snippets/ruby-mode/rspec-mode/let" nil nil)
                       ("it" "it '$1' do\n  $2\nend" "scenario it '...' do ... end" nil nil nil "/home/danilo/.emacs.d/private/snippets/ruby-mode/rspec-mode/it" nil nil)
                       ("expb" "expect { $1 }.to" "expectation block expect { .. }.to" nil nil nil "/home/danilo/.emacs.d/private/snippets/ruby-mode/rspec-mode/expb" nil nil)
                       ("exp" "expect($1).to $2" "expectation (object)" nil nil nil "/home/danilo/.emacs.d/private/snippets/ruby-mode/rspec-mode/exp" nil nil)
                       ("descs" "RSpec.describe '$1' do\n  $2\nend" "spec describe (string)" nil nil nil "/home/danilo/.emacs.d/private/snippets/ruby-mode/rspec-mode/descs" nil nil)
                       ("desci" "# frozen_string_literal: true\n\nrequire 'rails_helper'\n\nRSpec.describe $1 do\n  $2\nend\n" "init spec file" nil nil nil "/home/danilo/.emacs.d/private/snippets/ruby-mode/rspec-mode/desci" nil nil)
                       ("descc" "described_class\n" "described class" nil nil nil "/home/danilo/.emacs.d/private/snippets/ruby-mode/rspec-mode/descc" nil nil)
                       ("desc" "describe '$1' do\n  $2\nend" "describe scenario (string)" nil nil nil "/home/danilo/.emacs.d/private/snippets/ruby-mode/rspec-mode/desc" nil nil)
                       ("con" "context '$1' do\n $2\nend" "context (string)" nil nil nil "/home/danilo/.emacs.d/private/snippets/ruby-mode/rspec-mode/con" nil nil)
                       ("bef" "before do\n  $1\nend" "before { ... }" nil nil nil "/home/danilo/.emacs.d/private/snippets/ruby-mode/rspec-mode/bef" nil nil)
                       ("allrr" "allow($1).to receive($2).and_return($3)\n" "allow(...).to receive(...).and_return(...)" nil nil nil "/home/danilo/.emacs.d/private/snippets/ruby-mode/rspec-mode/allrr" nil nil)
                       ("allr" "allow($1).to receive($2)" "allow receive | allow(...).to receive" nil nil nil "/home/danilo/.emacs.d/private/snippets/ruby-mode/rspec-mode/allr" nil nil)))


;;; Do not edit! File generated at Wed Feb  5 00:01:35 2025
