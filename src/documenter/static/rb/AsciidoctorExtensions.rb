require 'asciidoctor'
require 'asciidoctor/extensions'

Asciidoctor::Extensions.register do
  inline_macro DeclInlineMacro
end

class DeclInlineMacro < Asciidoctor::Extensions::InlineMacroProcessor
  use_dsl

  named :decl

  def process parent, target, attrs
    path = target

    url = execute :declaration_url, target

    target = %(#{path}.html)
    parent.document.register :links, target
    %(#{(create_anchor parent, path, type: :link, target: target).convert}(#{url}))
  end
end

REQUEST_LOG_PATH = ENV['LUSHUI_REQUEST_LOG_PATH']
RESPONSE_LOG_PATH = ENV['LUSHUI_RESPONSE_LOG_PATH']

def execute command, payload
  message = "#{command} #{payload}"

  timestamp = Time.now
  identifier = "#{timestamp.to_i} #{timestamp.nsec}"
  request = "#{identifier}\x1F#{message}\x1E"

  File::open(REQUEST_LOG_PATH, 'a') { |log| log.write request }

  last_log_size = 0

  loop do
    sleep 0.1

    response_log = File::open(RESPONSE_LOG_PATH).read

    if response_log.length != last_log_size
      last_log_size = response_log.length

      _, message = response_log
        .split("\x1E")
        .to_enum
        .map { |response| response.split "\x1F", 2 }
        .find { |some_identifier, _| some_identifier == identifier }

      unless message.nil?
        return message
      end
    end
  end
end

