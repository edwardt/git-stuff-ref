module BeehiveClient
  module Command

    class Apps < Base

      attr_reader :app_name

      def self.description
        "List Apps"
      end

      def run
        parse_args
        get_token unless @token
        pp apps_list
      end

      def apps_list
        get("apps.json", {"token" => @token})
      end

    end

  end
end
