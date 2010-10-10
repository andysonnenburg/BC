ENV['JAVA_HOME'] = '/usr/lib/jvm/default-java'
ENV['SCALA_HOME'] = '/home/andy/Development/scala/dists/latest'
Buildr.settings.build['scala.version'] = '2.8.0'

require 'buildr/scala'

module Buildr
  module FindBugs
    include Extension
    VERSION = '1.3.9'

    class << self
      def version
        Buildr.settings.build['findbugs'] || VERSION
      end
    end

    REQUIRES = ["com.google.code.findbugs:findbugs-ant:jar:#{version}"]

    class << self
      def requires
        @requires ||= Buildr.transitive(REQUIRES).each(& :invoke).map(& :to_s)
      end
    end

    class FindBugsTask < Rake::Task

      attr_reader :project

      attr_writer :sourcePath, :auxAnalyzePath, :auxClasspath, :jvmargs, :report, :excludeFilter

      def initialize(* args) #:nodoc:
        super
        enhance([:compile]) do
          mkpath File.dirname(report) #otherwise Findbugs can't create the file

          Buildr.ant('findBugs') do |ant|
            antClasspath = FindBugs.requires.join(File::PATH_SEPARATOR)
            excludeFilterFile = File.expand_path(excludeFilter)

            ant.taskdef :name=>'findBugs',
            :classname=>'edu.umd.cs.findbugs.anttask.FindBugsTask',
            :classpath => antClasspath
            ant.findBugs :output => "xml", :outputFile => report, :classpath => antClasspath, :pluginList => '', :jvmargs => jvmargs, :excludeFilter => excludeFilterFile, :reportLevel => "low", :effort => "max" do
              ant.sourcePath :path => sourcePath
              ant.auxAnalyzePath :path => auxAnalyzePath
              ant.auxClasspath { |aux| auxClasspath.each { |dep| aux.pathelement :location => dep } } unless auxClasspath.empty?
            end
          end
        end
      end

      def report
        @report || project.path_to(:reports, 'findbugs.xml')
      end

      def sourcePath
        @sourcePath || project.compile.sources.select { |source| File.directory?(source.to_s) }.join(File::PATH_SEPARATOR)
      end

      def auxAnalyzePath
        @auxAnalyzePath || project.compile.target
      end

      def auxClasspath
        @auxClasspath || project.compile.dependencies
      end

      def jvmargs
        @jvmargs || "-Xmx512m"
      end

      def excludeFilter
        @excludeFilter || project.path_to("exclude_filter.xml")
      end

      # :call-seq:
      #   with(options) => self
      #
      # Passes options to the task and returns self.
      #
      def with(options)
        options.each do |key, value|
          begin
            send "#{key}=", value
          rescue NoMethodError
            raise ArgumentError, "#{self.class.name} does not support the option #{key}"
          end
        end
        self
      end

      private

      def associate_with(project)
        @project = project
      end
    end

    first_time do
      desc 'Run Findbugs'
      Project.local_task('findBugs') { |name| "Run Findbugs on #{name}" }
    end

    before_define do |project|
      task = FindBugsTask.define_task('findBugs')
      task.send :associate_with, project
      project.recursive_task('findBugs')
    end

    after_define do |project|
      project.clean do
        rm_rf project.path_to(:reports, "findbugs.xml")
      end
    end

    def findBugs(* deps, & block)
      task('findBugs').enhance deps, & block
    end
  end
end

class Buildr::Project
  include Buildr::FindBugs
end

module Buildr
  module CopyDependencies
    include Extension

    first_time do
      desc 'Copy dependencies'
      Project.local_task('copy-dependencies')
    end

    before_define do |project|
      project.recursive_task 'copy-dependencies' do |task|
        dir = 'target/dependencies'
        mkdir_p dir
        task.prerequisites.each do |path|
          cp path.to_s, dir
        end
      end
    end

    after_define do |project|
      task('copy-dependencies' => project.compile.dependencies)
    end
  end
end

class Buildr::Project
  include CopyDependencies
end

VERSION_NUMBER = '1.0.0'
GROUP = 'com.github.sonyandy'
COPYRIGHT = ''

repositories.remote << 'http://www.ibiblio.org/maven2'
repositories.remote << 'http://scala-tools/repo-releases'

ASM = artifact('asm:asm:jar:3.3').from file('asm/asm/output/dist/lib/asm-3.3.jar')
ASM_COMMONS = artifact('asm:asm-commons:jar:3.3').from file('asm/asm/output/dist/lib/asm-commons-3.3.jar')

JUNIT = 'junit:junit:jar:4.8.1'
SPECS = 'org.scala-tools.testing:specs_2.8.0:jar:1.6.5'

desc 'The BC project'
define 'bc' do
  compile.options.delete :lint
  compile.using :deprecation => true, :warnings => true, :optimise => true, :javac => { :lint => true }, :other => '-unchecked'
  compile.with ASM
  compile.with ASM_COMMONS

  test.with JUNIT
  test.with SPECS
  test.using :specs
  
  project.version = VERSION_NUMBER
  project.group = GROUP
  manifest['Implementation-Vendor'] = COPYRIGHT

  package :jar
end
